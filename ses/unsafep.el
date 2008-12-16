;;;; unsafep.el -- Determine whether a Lisp form is safe to evaluate

;; Copyright (C) 2002, 2003 Free Software Foundation, Inc.

;; Author: Jonathan Yavner <jyavner@member.fsf.org>
;; Maintainer: Jonathan Yavner <jyavner@member.fsf.org>
;; Keywords: safety lisp utility
;; Version: 2003-11-30

;; unsafep is part of GNU Emacs.  This file is intended for separate
;; download and use as part of Emacs revs 21.1-21.4 that do not include
;; unsafep.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a simplistic implementation that does not allow any modification of
;; buffers or global variables.  It does no dataflow analysis, so functions
;; like `funcall' and `setcar' are completely disallowed.  It is designed
;; for "pure Lisp" formulas, like those in spreadsheets, that don't make any
;; use of the text editing capabilities of Emacs.

;; A formula is safe if:
;;  1.  It's an atom.
;;  2.  It's a function call to a safe function and all arguments are safe
;;      formulas.
;;  3.  It's a special form whose arguments are like a function's (and,
;;	catch, if, or, prog1, prog2, progn, while, unwind-protect).
;;  4.  It's a special form or macro that creates safe temporary bindings
;;      (condition-case, dolist, dotimes, lambda, let, let*).
;;  4.  It's one of (cond, quote) that have special parsing.
;;  5.  It's one of (add-to-list, setq, push, pop) and the assignment variable
;;      is safe.
;;  6.  It's one of (apply, mapc, mapcar, mapconcat) and its first arg is a
;;      quoted safe function.
;;
;; A function is safe if:
;;  1.  It's a lambda containing safe formulas.
;;  2.  It's a member of list `safe-functions', so the user says it's safe.
;;  3.  It's a symbol with the `side-effect-free' property, defined by the
;;      byte compiler or function author.
;;  4.  It's a symbol with the `safe-function' property, defined here or by
;;      the function author.  Value t indicates a function that is safe but
;;      has innocuous side effects.  Other values will someday indicate
;;      functions with side effects that are not always safe.
;;  The `side-effect-free' and `safe-function' properties are provided for
;;  built-in functions and for functions and macros defined in subr.el.
;;
;; A temporary binding is unsafe if its symbol:
;;  1.  Has the `risky-local-variable' property.
;;  2.  Has a name that ends with -command, font-lock-keywords(-[0-9]+)?,
;;      font-lock-syntactic-keywords, -form, -forms, -frame-alist, -function,
;;       -functions, -history, -hook, -hooks, -map, -map-alist, -mode-alist,
;;       -predicate, or -program.
;;
;; An assignment variable is unsafe if:
;;   1. It would be unsafe as a temporary binding.
;;   2. It doesn't already have a temporary or buffer-local binding.

;; There are unsafe forms that `unsafep' cannot detect.  Beware of these:
;;   1. The form's result is a string with a display property containing a
;;      form to be evaluated later, and you insert this result into a
;;      buffer.  Always remove display properties before inserting!
;;   2. The form alters a risky variable that was recently added to Emacs and
;;      is not yet marked with the `risky-local-variable' property.
;;   3. The form uses undocumented features of built-in functions that have
;;      the `side-effect-free' property.  For example, in Emacs-20 if you
;;      passed a circular list to `assoc', Emacs would crash.  Historically,
;;      problems of this kind have been few and short-lived.

(provide 'unsafep)
(require 'byte-opt)  ;Set up the `side-effect-free' properties

(eval-when-compile
  (let ((major 21))
    (if (> emacs-major-version major)
	;;Need to recheck the Elisp manual for new risky variables
	(byte-compile-log-1 (format "This unsafep rev is for Emacs %d.x"
				    major)))))

(defvar safe-functions nil
  "*t to disable all safety checks, or a list of assumed-safe functions.")

(defvar unsafep-vars nil
  "Dynamically-bound list of variables that have lexical bindings at this
point in the parse.")
(put 'unsafep-vars 'risky-local-variable t)

;;Some side-effect-free subrs not listed in older versions of byte-opt.el
(when (and (= emacs-major-version 21) (< emacs-minor-version 3))
  (dolist (x '(bool-vector-p char-to-string compare-strings copy-alist
	       copy-sequence current-time current-time-string
	       current-time-zone decode-time encode-time error-message-string
	       fceiling ffloor float-time format-time-string fround
	       ftruncate intern-soft make-list make-string make-symbol
	       plist-get plist-member prin1-to-string rassoc
	       read-from-string region-beginning region-end safe-length
	       symbol-name truncate wholenump))
  (put x 'side-effect-free t)))

;;Side-effect-free functions from subr.el
(dolist (x '(assoc-default assoc-ignore-case butlast last match-string
	     match-string-no-properties member-ignore-case remove remq))
  (put x 'side-effect-free t))

;;Other safe functions
(dolist (x '(;;Special forms
	     and catch if or prog1 prog2 progn while unwind-protect
	     ;;Safe subrs that have some side-effects
	     ding error message minibuffer-message random read-minibuffer
	     signal sleep-for string-match throw y-or-n-p yes-or-no-p
	     ;;Defsubst functions from subr.el
	     caar cadr cdar cddr
	     ;;Macros from subr.el
	     save-match-data unless when with-temp-message
	     ;;Functions from subr.el that have side effects
	     read-passwd split-string replace-regexp-in-string
	     play-sound-file))
  (put x 'safe-function t))

;;Risky variables not listed in files.el
(dolist (x '(default-directory font-lock-defaults format-alist inhibit-quit))
  (put x 'risky-local-variable t))

;;Risky variables that are not listed in older versions of files.el
(when (and (= emacs-major-version 21) (< emacs-minor-version 3))
  (dolist (x '(buffer-undo-list default-text-properties frame-title-format
	       global-mode-string header-line-format icon-title-format
	       imenu-generic-expression imenu-index-alist input-method-alist
	       max-lisp-eval-depth max-specpdl-size
	       overriding-terminal-local-map special-display-buffer-names
	       standard-input standard-output unread-command-events vc-mode))
    (put x 'risky-local-variable t)))


;;;###autoload
(defun unsafep (form &optional unsafep-vars)
  "Return nil if evaluating FORM couldn't possibly do any harm; otherwise
result is a reason why FORM is unsafe.  UNSAFEP-VARS is a list of symbols
with local bindings."
  (catch 'unsafep
    (if (or (eq safe-functions t)	    ;User turned off safety-checking
	    (atom form))		    ;Atoms are never unsafe
	(throw 'unsafep nil))
    (let* ((fun    (car form))
	   (reason (unsafep-function fun))
	   arg)
      (cond
       ((not reason)
	;;It's a normal function - unsafe if any arg is
	(unsafep-progn (cdr form)))
       ((eq fun 'quote)
	;;Never unsafe
	nil)
       ((memq fun '(apply mapc mapcar mapconcat))
	;;Unsafe if 1st arg isn't a quoted lambda
	(setq arg (cadr form))
	(cond
	 ((memq (car-safe arg) '(quote function))
	  (setq reason (unsafep-function (cadr arg))))
	 ((eq (car-safe arg) 'lambda)
	  ;;Self-quoting lambda
	  (setq reason (unsafep arg unsafep-vars)))
	 (t
	  (setq reason `(unquoted ,arg))))
	(or reason (unsafep-progn (cddr form))))
       ((eq fun 'lambda)
	;;First arg is temporary bindings
	(mapc #'(lambda (x)
		  (let ((y (unsafep-variable x t)))
		    (if y (throw 'unsafep y)))
		  (or (memq x '(&optional &rest))
		      (push x unsafep-vars)))
	      (cadr form))
	(unsafep-progn (cddr form)))
       ((eq fun 'let)
	;;Creates temporary bindings in one step
	(setq unsafep-vars (nconc (mapcar #'unsafep-let (cadr form))
				  unsafep-vars))
	(unsafep-progn (cddr form)))
       ((eq fun 'let*)
	;;Creates temporary bindings iteratively
	(dolist (x (cadr form))
	  (push (unsafep-let x) unsafep-vars))
	(unsafep-progn (cddr form)))
       ((eq fun 'setq)
	;;Safe if odd arguments are local-var syms, evens are safe exprs
	(setq arg (cdr form))
	(while arg
	  (setq reason (or (unsafep-variable (car arg) nil)
			   (unsafep (cadr arg) unsafep-vars)))
	  (if reason (throw 'unsafep reason))
	  (setq arg (cddr arg))))
       ((eq fun 'pop)
	;;safe if arg is local-var sym
	(unsafep-variable (cadr form) nil))
       ((eq fun 'push)
	;;Safe if 2nd arg is a local-var sym
	(or (unsafep (cadr form) unsafep-vars)
	    (unsafep-variable (nth 2 form) nil)))
       ((eq fun 'add-to-list)
	;;Safe if first arg is a quoted local-var sym
	(setq arg (cadr form))
	(if (not (eq (car-safe arg) 'quote))
	    `(unquoted ,arg)
	  (or (unsafep-variable (cadr arg) nil)
	      (unsafep-progn (cddr form)))))
       ((eq fun 'cond)
	;;Special form with unusual syntax - safe if all args are
	(dolist (x (cdr form))
	  (setq reason (unsafep-progn x))
	  (if reason (throw 'unsafep reason))))
       ((memq fun '(dolist dotimes))
	;;Safe if COUNT and RESULT are safe.  VAR is bound while checking BODY.
	(setq arg (cadr form))
	(or (unsafep-progn (cdr arg))
	    (let ((unsafep-vars (cons (car arg) unsafep-vars)))
	      (unsafep-progn (cddr form)))))
       ((eq fun 'condition-case)
	;;Special form with unusual syntax - safe if all args are
	(or (unsafep-variable (cadr form) t)
	    (unsafep (nth 2 form) unsafep-vars)
	    (let ((unsafep-vars (cons (cadr form) unsafep-vars)))
	      ;;var is bound only during handlers
	      (dolist (x (nthcdr 3 form))
		(setq reason (unsafep-progn (cdr x)))
		(if reason (throw 'unsafep reason))))))
       (t
	;;First unsafep-function call above wasn't nil, no special case applies
	reason)))))


(defun unsafep-function (fun)
  "Return nil if FUN is a safe function (either a safe lambda or a
symbol that names a safe function).  Otherwise result is a reason code."
  (cond
   ((eq (car-safe fun) 'lambda)
    (unsafep fun unsafep-vars))
   ((not (and (symbolp fun)
	      (or (get fun 'side-effect-free)
		  (eq (get fun 'safe-function) t)
		  (eq safe-functions t)
		  (memq fun safe-functions))))
    `(function ,fun))))

(defun unsafep-progn (list)
  "Return nil if all forms in LIST are safe, or the reason for the first
unsafe form."
  (catch 'unsafep-progn
    (let (reason)
      (dolist (x list)
	(setq reason (unsafep x unsafep-vars))
	(if reason (throw 'unsafep-progn reason))))))

(defun unsafep-let (clause)
  "CLAUSE is a let-binding, either SYM or (SYM) or (SYM VAL).  Throws a
reason to `unsafep' if VAL isn't safe.  Returns SYM."
  (let (reason sym)
    (if (atom clause)
	(setq sym clause)
      (setq sym    (car clause)
	    reason (unsafep (cadr clause) unsafep-vars)))
    (setq reason (or (unsafep-variable sym t) reason))
    (if reason (throw 'unsafep reason))
    sym))

(defun unsafep-variable (sym global-okay)
  "Returns nil if SYM is lexically bound or is a non-risky buffer-local
variable, otherwise a reason why it is unsafe.  Failing to be locally bound
is okay if GLOBAL-OKAY is non-nil."
  (cond
   ((not (symbolp sym))
    `(variable ,sym))
   ((or (get sym 'risky-local-variable)
	;;Files.el has a similar list, but without -map or -history
	(string-match
	 "-command$\\|font-lock-keywords$\\|font-lock-keywords-[0-9]+$\\|font-lock-syntactic-keywords$\\|-forms?$\\|-frame-alist$\\|-functions?$\\|-history$\\|-hooks?$\\|-map$\\|-map-alist$\\|-mode-alist$\\|-predicate$\\|-program$"
		      (symbol-name sym)))
    `(risky-local-variable ,sym))
   ((not (or global-okay
	     (memq sym unsafep-vars)
	     (local-variable-p sym)))
    `(global-variable ,sym))))

;; unsafep.el ends here.
