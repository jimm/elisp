;;;; unsafep-coverage.el -- Use testcover to test unsafep's code coverage

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

(require 'testcover)

;;;These forms are all considered safe
(defconst unsafep-coverage-safe
  '(((lambda (x) (* x 2)) 14)
    (apply 'cdr (mapcar '(lambda (x) (car x)) y))
    (cond ((= x 4) 5) (t 27))
    (condition-case x (car y) (error (car x)))
    (dolist (x y) (message "here: %s" x))
    (dotimes (x 14 (* x 2)) (message "here: %d" x))
    (let (x) (dolist (y '(1 2 3) (1+ y)) (push y x)))
    (let (x) (apply '(lambda (x) (* x 2)) 14))
    (let ((x '(2))) (push 1 x) (pop x) (add-to-list 'x 2))
    (let ((x 1) (y 2)) (setq x (+ x y)))
    (let ((x 1)) (let ((y (+ x 3))) (* x y)))
    (let* nil (current-time))
    (let* ((x 1) (y (+ x 3))) (* x y))
    (mapcar (lambda (x &optional y &rest z) (setq y (+ x 2)) (* y 3)) '(1 2 3))
    (mapconcat #'(lambda (var) (propertize var 'face 'bold)) '("1" "2") ", ")
    (setq buffer-display-count 14 mark-active t)
    ;;This is not safe if you insert it into a buffer!
    (propertize "x" 'display '(height (progn (delete-file "x") 1))))
  "List of forms that `unsafep' should decide are safe.")

;;;These forms are considered unsafe
(defconst unsafep-coverage-unsafe
  '(( (add-to-list x y)
      . (unquoted x))
    ( (add-to-list y x)
      . (unquoted y))
    ( (add-to-list 'y x)
      . (global-variable y))
    ( (not (delete-file "unsafep.el"))
      . (function delete-file))
    ( (cond (t (aset local-abbrev-table 0 0)))
      . (function aset))
    ( (cond (t (setq default-directory "")))
      . (risky-local-variable default-directory))
    ( (condition-case format-alist 1)
      . (risky-local-variable format-alist))
    ( (condition-case x 1 (error (setq default-directory "")))
      . (risky-local-variable default-directory))
    ( (dolist (x (sort globalvar 'car)) (princ x))
      . (function sort))
    ( (dotimes (x 14) (delete-file "x"))
      . (function delete-file))
    ( (let ((post-command-hook "/tmp/")) 1)
      . (risky-local-variable post-command-hook))
    ( (let ((x (delete-file "x"))) 2)
      . (function delete-file))
    ( (let (x) (add-to-list 'x (delete-file "x")))
      . (function delete-file))
    ( (let (x) (condition-case y (setq x 1 z 2)))
      . (global-variable z))
    ( (let (x) (condition-case z 1 (error (delete-file "x"))))
      . (function delete-file))
    ( (let (x) (mapc (lambda (x) (setcar x 1)) '((1 . 2) (3 . 4))))
      . (function setcar))
    ( (let (y) (push (delete-file "x") y))
      . (function delete-file))
    ( (let* ((x 1)) (setq y 14))
      . (global-variable y))
    ( (mapc 'car (list '(1 . 2) (cons 3 4) (kill-buffer "unsafep.el")))
      . (function kill-buffer))
    ( (mapcar x y)
      . (unquoted x))
    ( (mapcar '(lambda (x) (rename-file x "x")) '("unsafep.el"))
      . (function rename-file))
    ( (mapconcat x1 x2 " ")
      . (unquoted x1))
    ( (pop format-alist)
      . (risky-local-variable format-alist))
    ( (push 1 format-alist)
      . (risky-local-variable format-alist))
    ( (setq buffer-display-count (delete-file "x"))
      . (function delete-file))
    ;;These are actualy safe (they signal errors)
    ( (apply '(x) '(1 2 3))
      . (function (x)))
    ( (let (((x))) 1)
      . (variable (x)))
    ( (let (1) 2)
      . (variable 1))
    )
  "A-list of (FORM . REASON)... that`unsafep' should decide are unsafe.")


;;;###autoload
(defun unsafep-test ()
  "Executes all unsafep tests and displays the coverage results."
  (interactive)
  (testcover-unmark-all "unsafep.el")
  (testcover-start "unsafep.el")
  (let (save-functions)
    (dolist (x unsafep-coverage-safe)
      (if (unsafep x)
	  (error "%S should be safe" x)))
    (dolist (x unsafep-coverage-unsafe)
      (if (not (equal (unsafep (car x)) (cdr x)))
	  (error "%S should be unsafe: %s" (car x) (cdr x))))
    (setq safe-functions t)
    (if (or (unsafep '(delete-file "x"))
	    (unsafep-function 'delete-file))
	(error "safe-functions=t should allow delete-file"))
    (setq safe-functions '(setcar))
    (if (unsafep '(setcar x 1))
	(error "safe-functions=(setcar) should allow setcar"))
    (if (not (unsafep '(setcdr x 1)))
	(error "safe-functions=(setcar) should not allow setcdr")))
  (testcover-mark-all "unsafep.el")
  (testcover-end "unsafep.el")
  (message "Done"))

;; unsafep-coverage.el ends here.
