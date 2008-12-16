;;; remember-blosxom --- Blosxom support for remember.el

;; Copyright 2004 Gary V. Vaughan (gary AT gnu DOT org)

;; Emacs Lisp Archive Entry
;; Filename: remember-blosxom.el
;; Version: 0.1
;; Date: Wed, 19 May 2004
;; Keywords: memory blog
;; Author: Gary V. Vaughan (gary AT gnu DOT org)
;; Maintainer: Gary V. Vaughan (gary AT gnu DOT org)
;; Description: Remember to an emacs-wiki-blosxom.el story
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

;; I maintain the hypertext parts of my website with John Wiegley's
;; emacs-wiki.el, now maintained by Sacha Chua at
;; http://sacha.free.net/notebook/emacs/emacs-wiki.  You will need to
;; install a copy of that file and remember.el from that site, and also
;; emacs-wiki-blosxom.el, and read-file-name.el from the same place that
;; you got this file before this can be of any use to you.

;; To use, place this in your .emacs
;;
;; (require 'remember-blosxom)
;; (setq remember-handler-functions '(remember-blosxom))
;; (setq remember-annotation-functions nil)
;;
;; Then type M-x remember       to remember new text, or
;;           C-u M-x remember   to remember a region

;;; Code:

(require 'emacs-wiki-blosxom)
(require 'read-file-name)
(require 'remember)

(defvar remember-blosxom-hook '(remember-blosxom-add-timestamp)
  "*Functions to run after something has been appended to the blosxom page.")

(defvar remember-blosxom-timestamp-format "#date %Y-%m-%d %H:%M"
  "*Format of timestamp for remember entries.")

(defun remember-blosxom-add-timestamp ()
  "Add a timestamp at the start of the buffer if there is not one already.
This function can be added to `remember-blosxom-hook'."
  (goto-char (point-min))
  (unless (save-excursion (save-match-data
			    (re-search-forward "^#date " nil t)))
    (insert (format-time-string remember-blosxom-timestamp-format
				(current-time)) "\n")))


;;;###autoload
(defun remember-blosxom ()
  "Remember this text to a blosxom story.
This function can be added to `remember-handler-functions'."
  (require 'emacs-wiki-blosxom)
  (let* ((text (buffer-string))
	 (category-file (remember-blosxom-read-new-wiki-page))
	 (dir (file-name-directory category-file)))
    (or (file-exists-p dir)
	(make-directory dir 'parents))
    (with-temp-buffer
      (insert text)
      (if (not (bolp))
	  (insert "\n"))
      (run-hooks 'remember-blosxom-hook)
      (write-file category-file)
      (blosxom-mode))
    (let ((buffer (find-file category-file)))
      (emacs-wiki-publish-this-page)
      (kill-buffer buffer))
    t))

(custom-add-option 'remember-handler-functions 'remember-blosxom)

;; Used to pass the current prompt string from
;; `remember-blosxom-read-new-wiki-page' to `remember-blosxom-predicate'
(defvar remember-blosxom-prompt nil)

;; Used to pass the current completion directory from
;; `remember-blosxom-read-new-wiki-page' to `remember-blosxom-predicate'
(defvar remember-blosxom-category-directory nil)

;; Used to remember previous user entries to
;; `remember-blosxom-read-new-wiki-page'
(defvar remember-blosxom-history-list nil)

(defun remember-blosxom-read-new-wiki-page (&optional prompt default)
  "Read a new wiki page name in the mini-buffer with completion.
Entries that refer to directories or existing wiki page names are not
accepted."
  (with-emacs-wiki-project blosxom-project
    (let* ((insert-default-directory nil)
	   (dir (car emacs-wiki-directories))
	   (prompt (or prompt "Blosxom Category: "))
	   (remember-blosxom-prompt prompt)
	   (remember-blosxom-category-directory dir)
	   initial file-name)
      ;; while we read the name of an existing directory or wiki page...
      (while (progn
	       (setq initial (completing-read-file-name
			      prompt dir default nil initial
			      'remember-blosxom-history-list
			      'remember-blosxom-predicate)
		     file-name (concat (directory-file-name dir)
				       "/" initial))
		 (file-exists-p file-name))
	(message "%s%s [%s]" prompt initial
		 (if (file-directory-p file-name) "Directory" "File Exists"))
	;; ...break up the result for the next iteration and try again
	(setq initial (cons initial (1+ (length initial)))
	      remember-blosxom-category-directory
	      (file-name-directory file-name))
	(sit-for 1))
      (concat (directory-file-name dir) "/"
	      (if (consp initial) (car initial) initial)))))

(defun remember-blosxom-predicate (cons-cell)
  "Return nil if the car of CONS-CELL is an existing (non-directory) file.
This function is used as the PREDICATE argument of `completing-read-file-name'
when called from `remember-blosxom-read-new-wiki-page' to prune out existing
wiki page names from the possible completions."
  (let* ((directory remember-blosxom-category-directory)
	 (prompt remember-blosxom-prompt)
	 (prompt-re (concat "^" (regexp-quote prompt)))
	 (raw-string (buffer-string))
	 (field-start (progn (string-match prompt-re raw-string)
			     (match-end 0)))
	 (minibuffer-string (substring raw-string field-start))
	 (prefix-string (file-name-directory minibuffer-string))
	 (file-name (concat (directory-file-name directory)
			    "/" prefix-string (car cons-cell))))
    (not (and (file-exists-p file-name) (not (file-directory-p file-name))))))

(provide 'remember-blosxom)

;;; remember-blosxom.el ends here
