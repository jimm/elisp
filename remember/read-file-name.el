;;; read-file-name --- improved read-file-name with better callbacks

;; Copyright 2004 Gary V. Vaughan (gary AT gnu DOT org)

;; Emacs Lisp Archive Entry
;; Filename: read-file-name.el
;; Version: 0.3
;; Date: Wed, 27 May 2004
;; Keywords: minibuffer file-name completion
;; Author: Gary V. Vaughan (gary AT gnu DOT org)
;; Maintainer: Gary V. Vaughan (gary AT gnu DOT org)
;; Description: Improved read-file-name with better callbacks
;; URL: http://tkd.kicks-ass.net/arch/gary@gnu.org--2004/remember--gary--1.0

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; There are no high level functions to read the name of a non-existent
;; file from the minibuffer in emacs.  CVS GNU Emacs has added two new
;; parameters to `read-file-name' to make writing such a function easier:
;; the new 6th parameter is HIST, to match XEmacs' footprint for that
;; function; the new 7th parameter is PREDICATE, the name of a predicate
;; used to test each candidate file.
;;
;; This file implements `completing-read-file-name', designed to be a
;; semantic equivalent to CVS GNU Emacs' 7 parameter `read-file-name'.

;; For example, to implement the reading of a non-existent file:
;;
;; (require 'read-file-name)
;;
;; (defvar file-not-exists-directory nil)
;;
;; (defun read-file-not-exists (prompt &optional dir default hist)
;;   (let ((file-not-exists-directory
;; 	    (abbreviate-file-name
;; 	     (or dir (file-name-directory (buffer-file-name)))))
;; 	   initial)
;;     (while (file-exists-p
;; 	      (setq initial
;; 		    (completing-read-file-name prompt dir default nil initial
;; 					       hist 'file-not-exists-p)))
;;       (setq dir (file-name-directory initial)
;; 	       initial (file-name-nondirectory initial))
;;       (message "%s%s%s [File exists]" prompt dir initial)
;;       (sit-for 1))
;;     initial))
;;
;; (defun file-not-exists-p (cons-cell)
;;   (not (file-exists-p (concat file-not-exists-directory (car cons-cell)))))

;;; Code:

(defvar completing-read-file-name-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "?"       'completing-read-file-name-complete-help)
    (define-key map " "       'completing-read-file-name-complete-word)
    (define-key map "\C-i"    'completing-read-file-name-complete)
    (define-key map "\C-g"    'abort-recursive-edit)
    (define-key map "\C-m"    'exit-minibuffer)
    (define-key map "\C-j"    'exit-minibuffer)
    (define-key map [down]    'next-history-element)
    (define-key map [up]      'previous-history-element)
    (define-key map "\M-n"    'next-history-element)
    (define-key map "\M-p"    'previous-history-element)
    map)
  "Keymap for reading a file name from the minibuffer with completion.")

(defvar completing-read-file-name-directory nil
  "The path to the root directory of this completion attempt.")

(defvar completing-read-file-name-prompt nil
  "The prompt string to show in the minibuffer during reading.")

(defvar completing-read-file-name-predicate nil
  "The predicate function used to narrow the completion alist.")


(defun completing-read-file-name (prompt &optional dir default
					 mustmatch initial hist predicate)
  "Read file name, prompting with PROMPT and completing in directory DIR.
DIR defaults to current buffer's directory default.
If `insert-default-directory' is non-nil then DIR is inserted before point
 in the minibuffer, ready for editing.
Default name to DEFAULT if user enters a null string.
 (If DEFAULT is omitted, the visited file name is used,
  except that if INITIAL is specified, that combined with DIR is used.)
Optional fourth arg MUSTMATCH non-nil means require existing file's name.
 Non-nil and non-t means also require confirmation after completion.
Optional fifth arg INITIAL specifies text to start with.
Optional sixth arg HIST, if non-nil, is a symbol that is the history list
 variable to use.
If Optional seventh argument is non-nil, it is used to test each possible
 match.  The match is a candidate only if PREDICATE returns non-nil.
 See `try-completion' for more details on PREDICATE,
Value is not expanded---you must call `expand-file-name' yourself."
  (let ((keymap completing-read-file-name-keymap)
	(directory (abbreviate-file-name
		    (or dir (file-name-directory (buffer-file-name)))))
	file-name)
    (while
	(let ((completing-read-file-name-directory directory)
	      (completing-read-file-name-prompt prompt)
	      (completing-read-file-name-predicate predicate)
	      default-file-name)
	  (setq default-file-name (if insert-default-directory
				      (cons (concat directory initial)
					    (1+ (length directory)))
				    initial))
	  (setq file-name
		(read-from-minibuffer prompt default-file-name
				      keymap nil hist default))
	  (when mustmatch
	    (unless (file-exists-p file-name)
	      (message "%s%s%s [No match]" prompt
		       (if insert-default-directory directory "") initial)
	      (setq file-name (file-relative-name file-name directory))
	      (sit-for 2)))))
    file-name))


(defvar completing-read-file-name-complete-word nil
  "Whether to limit this completion attempt to word constituent characters.")

(defun completing-read-file-name-complete-word ()
  (interactive)
  (let ((completing-read-file-name-complete-word t))
    (completing-read-file-name-complete)))


(defun completing-read-file-name-complete ()
  (interactive)
  (save-match-data
    (let* ((prompt completing-read-file-name-prompt)
	   (prompt-re (concat "^" (regexp-quote prompt)))
	   (predicate completing-read-file-name-predicate)
	   (raw-string (buffer-string))
	   (field-start (progn (string-match prompt-re raw-string)
			       (match-end 0)))
	   (minibuffer-string (substring raw-string field-start))
	   (prefix-string (file-name-directory minibuffer-string))
	   (directory
	    (substitute-in-file-name
	     (concat (directory-file-name completing-read-file-name-directory)
		     "/" prefix-string)))
	   (file-name (file-name-nondirectory minibuffer-string))
	   (alist (completing-read-file-name-complete-alist directory))
	   (ignore-re (completing-read-file-name-ignored-extensions))
	   (completion (progn
			 (dolist (elt alist)
			   (if (string-match ignore-re (car elt))
			       (assq-delete-all (car elt) alist)))
			 (try-completion file-name alist predicate)))

	   ;; try-completion returns t if file-name is a unique exact match
	   (exact-p (eq completion t)))

      (when exact-p
	(setq completion file-name)
	(message "%s%s%s [Sole completion]" prompt
		 prefix-string file-name)
	(sit-for 1))

      ;; add a trailing slash to directory names if necessary
      (if (and (< 0 (length completion))
	       (not (string-equal "/" (substring completion -1)))
	       (file-directory-p
		(substitute-in-file-name (concat directory completion))))
	  (setq completion (concat completion "/")))

      ;; if there is no change to the minibuffer go in to help
      (let ((completion-string (or completion "")))
	(if (and (not exact-p) (string= file-name completion-string))
	    (completing-read-file-name-complete-help)))

      ;; warn if the minibuffer cannot be completed from ALIST
      (when (null completion)
	(message "%s%s%s [No match]" prompt
		 prefix-string file-name)
	(sit-for 1))

      ;; limit completion to word constituent chars only if necessary
      ;; this normally fires when completing on word boundaries with [SPC]
      (when (and completing-read-file-name-complete-word
		 (> (length completion) (length file-name)))
	(string-match (concat "^" (regexp-quote file-name)) completion)
	(let ((additional (substring completion (match-end 0))))
	  (string-match "\\(^\\w*\\)\\(\\W\\|$\\)" additional)
	  (setq completion
		(concat file-name
			(if (string= "" (match-string 1 additional))
			    (match-string 2 additional)
			  (match-string 1 additional))))))

      ;; put the new contents back in the minibuffer
      (if (> (length completion) (length file-name))
	  (setq file-name (substitute-in-file-name completion)))
      (delete-region (1+ field-start) (point-max))
      (insert (concat prefix-string file-name)))))


(defun completing-read-file-name-ignored-extensions ()
  "Return a regexp to match files from `completion-ignored-extensions'."
  (let (re ext)
    (dolist (ext completion-ignored-extensions)
      (setq re (concat (and re (concat re "\\|")) (regexp-quote ext))))
    (format "\\(%s\\)$" re)))


(defun completing-read-file-name-complete-help ()
  "Show the available completions for current minibuffer contents."
  (interactive)
  (save-match-data
    (let* ((prompt completing-read-file-name-prompt)
	   (prompt-re (concat "^" (regexp-quote prompt)))
	   (raw-string (buffer-string))
	   (field-start (progn (string-match prompt-re raw-string)
			       (match-end 0)))
	   (minibuffer-string (substring raw-string field-start))
	   (prefix-string (file-name-directory minibuffer-string))
	   (directory
	    (substitute-in-file-name
	     (concat (directory-file-name completing-read-file-name-directory)
		     "/" prefix-string)))
	   (file-name (file-name-nondirectory minibuffer-string))
	   (alist (completing-read-file-name-complete-alist directory)))

      (with-output-to-temp-buffer "*Completions*"
	(display-completion-list
	 (all-completions file-name alist))))))


(defun completing-read-file-name-complete-alist (dir)
  "Generate an alist of files in DIR suitable for `try-completion'."
  (let ((file-list (directory-files (expand-file-name dir)))
	(i 1)
	alist)
    (dolist (file file-list)
      ;; skip . and ..
      (unless (or (string-equal file ".") (string-equal file ".."))
	;; add / to directories
	(when (file-directory-p (concat (directory-file-name dir) "/" file))
	    (setq file (concat file "/")))
	(let ((next (list (cons file i))))
	  (setq alist (if alist (append alist next) next)
		i (1+ i)))))
    alist))

(provide 'read-file-name)

;;; read-file-name.el ends here
