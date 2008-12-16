;;; remember-emacs-wiki-journal.el --- EmacsWiki journal support for remember

;; Copyright (C) 2004 Hoan Ton-That

;; Author: Hoan Ton-That <hoan@ton-that.org>
;; Maintainer: Hoan Ton-That <hoan@ton-that.org>
;; Created: 11 Jul 2004
;; Version: 0.1
;; Keywords: hypermedia remember wiki blog journal
;; URL: http://www.ton-that.org/elisp/remember-emacs-wiki-journal.el

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

;;;_* Commentary

;;;_ + Loading

;; When loading remember-emacs-wiki-journal it is a good idea to make
;; remember call all hooks

;; (require 'remember-emacs-wiki-journal)
;; (setq remember-all-handler-functions t)

;;;_ + Hooks

;; There are three hooks for remember:
;;   * `remember-emacs-wiki-journal-add-entry';
;;   * `remember-emacs-wiki-journal-add-entry-auto'; and
;;   * `remember-emacs-wiki-journal-add-entry-maybe'.

;; When `remember-emacs-wiki-journal-add-entry' is called, you are
;; prompted for a category and title.  Just fill those in and the
;; entry will be added.

;; The above hook is clumsy because of the prompt.
;; `remember-emacs-wiki-journal-add-entry-auto' gets rid of the
;; prompt completely.  It assumes the category is the first word and
;; the title is the rest of the words on that line. 

;; Sometimes you do not want to save *all* your notes to your journal.
;; `emacs-wiki-journal-add-entry-maybe' is similar to the previous
;; hook but only adds the entry if the first word matches
;; `emacs-wiki-journal-category-regexp'.  Thus entries will only be
;; added if the first word starts with Category.

;;;_ + Example

;; (require 'remember-emacs-wiki-journal)
;; (setq remember-all-handler-functions t)
;; (add-to-list 'remember-handler-functions
;; 	        'remember-emacs-wiki-journal-add-entry-maybe)

;; Now when C-c C-c is pressed in remember mode, it checks to see if
;; the buffer is a journal entry.  If it is it is added to the
;; journal.  The category is the first word and the title the
;; remaining words of the first line.  The first word must match
;; `emacs-wiki-journal-category-regexp'.  For example, a remember
;; buffer like this would be added:

;;   CategoryMisc Title of entry
;;
;;   Contents of entry

;;;_* Prerequisites
(require 'emacs-wiki-journal)
(require 'remember)

;;;_* Options
(defcustom remember-emacs-wiki-journal-add-hook '()
  "Functions to run after something has been added to the journal.
Buffer will be narrowed to the current entry."
  :type 'hook
  :group 'remember)

;;;_* Internal Functions

;; From remember-planner.el
(defun remember-emacs-wiki-journal-add (category heading)
  "Remember this text to WikiJournal under category with heading"
  (require 'emacs-wiki-journal)
  (set-buffer (get-buffer-create remember-buffer))
  (let ((text (buffer-string))
        start)
    (save-window-excursion
      (emacs-wiki-journal-add category heading)
      (setq start (planner-line-beginning-position))
      (insert text)
      (save-restriction
        (narrow-to-region start (point))
        (mapcar
         (lambda (hook)
           (save-window-excursion
             (save-excursion
               (save-restriction
                 (funcall hook)))))
         remember-emacs-wiki-journal-add-hook))
      (when remember-save-after-remembering
        (save-buffer)))
    t))

;;;_* User Functions

;;;###autoload
(defun remember-emacs-wiki-journal-add-entry ()
  "Prompt for category and heading and add entry."
  (let ((category (emacs-wiki-journal-prompt-for-category-wiki))
	(heading (read-from-minibuffer "Journal Heading: ")))
    (remember-emacs-wiki-journal-add category heading)))

;;;###autoload
(defun remember-emacs-wiki-journal-add-entry-auto ()
  "Add entry where the category is the first word and the heading the
rest of the words on the first line."
  (set-buffer (get-buffer-create remember-buffer))
  (goto-char (point-min))
  (let* ((text (buffer-string))
	 (split-first-line (split-string (thing-at-point 'line)))
	 (category (car split-first-line))
	 (heading (mapconcat 'concat
			     (cdr split-first-line)
			     " ")))
    (narrow-to-region (planner-line-beginning-position 2)
		      (point-max))
    (remember-emacs-wiki-journal-add category heading)
    (widen)
    t))

;;;###autoload
(defun remember-emacs-wiki-journal-add-entry-maybe ()
  "Like `remember-emacs-wiki-journal-add-entry-auto' but only adds
entry if the first line matches `emacs-wiki-journal-category-regexp'."
  (set-buffer (get-buffer-create remember-buffer))
  (goto-char (point-min))
  (when (string-match emacs-wiki-journal-category-regexp
		      (thing-at-point 'line))
    (remember-emacs-wiki-journal-add-entry-auto)))

;;;_* Initialization
(custom-add-option 'remember-handler-functions
		   'remember-emacs-wiki-journal-add-entry)
(custom-add-option 'remember-handler-functions
		   'remember-emacs-wiki-journal-add-entry-auto)
(custom-add-option 'remember-handler-functions
		   'remember-emacs-wiki-journal-add-entry-maybe)

(provide 'remember-emacs-wiki-journal)

;;;_* Local Emacs Vars.
;; Local variables:
;; allout-layout: (* 0 : )
;; End:

;;; remember-emacs-wiki-journal.el ends here
