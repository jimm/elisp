;;; remember-planner --- Planner support for remember.el

;; Copyright (C) 1999, 2000, 2001 John Wiegley
;; Copyright (C) 2003 Sandra Jean Chua

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Sacha Chua <sacha@free.net.ph>
;; Created: 29 Mar 1999
;; Keywords: data memory todo pim blog
;; URL: http://gna.org/projects/remember-el/

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

;; To use, place this in your .emacs
;;
;; (require 'remember-planner)
;; (setq remember-handler-functions '(remember-planner-append))
;;
;; You might also want to
;;   (setq remember-annotation-functions planner-annotation-functions)
;; or
;;   (defvaralias 'remember-annotation-functions 'planner-annotation-functions)
;;
;; Then type M-x remember       to remember new text, or
;;           C-u M-x remember   to remember a region

;;; Code:

(require 'planner)
(require 'remember)

(defcustom remember-planner-xref-p t
  "*Non-nil means cross-reference entries with a plan page."
  :type 'boolean
  :group 'remember)

(defcustom remember-planner-copy-on-xref-flag t
  "*Non-nil means copy note text instead of moving it to the plan page.
WARNING: If you set this to nil, make sure you do not call
planner-update-note from the page without the note body."
  :type 'boolean
  :group 'remember)

(defcustom remember-planner-timestamp-format " %H:%M"
  "*Format of timestamp for remember entries.
If you don't want timestamps, just set this to \"\"."
  :type 'string
  :group 'remember)

(defcustom remember-planner-append-hook '(remember-planner-add-timestamp)
  "Functions to run after something has been appended to the planner day page.
Buffer will be narrowed to the current note."
  :type 'hook
  :group 'remember)

(defcustom remember-planner-page nil
  "Planner page to save to by default.
This defaults to the current planner page."
  :type '(choice (string :tag "Planner page")
                 (const :tag "Current page" nil))
  :group 'remember)

;;;###autoload
(defun remember-planner-append (&optional page)
  "Remember this text to PAGE or today's page.
This function can be added to `remember-handler-functions'."
  (unless (or page (not planner-use-plan-pages))
    (setq page
          (let ((planner-default-page (or remember-planner-page (planner-today))))
            (when (or (not planner-use-day-pages)
                      remember-planner-xref-p)
              (planner-read-name (planner-file-alist))))))
  (let ((text (buffer-string))
        start)
    (save-window-excursion
      (if planner-use-day-pages
          (planner-create-note (planner-today))
        (planner-create-note page))
      (setq start (planner-line-beginning-position))
      (insert text)
      (unless (bolp) (insert "\n\n")) ;; trailing newline
      (save-restriction
        (narrow-to-region start (point))
        (when remember-planner-xref-p
          (remember-planner-add-xref page))
        (mapcar
         (lambda (hook)
           (save-window-excursion
             (save-restriction
               (funcall hook))))
         remember-planner-append-hook)
        ;; Should be back on today's page
        (unless (or remember-planner-copy-on-xref-flag
                    (null page)
                    (if planner-use-day-pages
                        (string= page (planner-today))
                      t))
          (delete-region (planner-line-end-position) (point-max))))
      (when remember-save-after-remembering
        (save-buffer)))
    t))

(defun remember-planner-add-timestamp ()
  "Add a timestamp to the current entry.
This function can be added to `remember-planner-append-hook'."
  (goto-char (point-min))
  (goto-char (planner-line-end-position))
  (skip-syntax-backward " ")
  (when (= (char-before (point)) ?)) ; Cross-referenced
    (search-backward "(" (planner-line-beginning-position) t))
  (skip-syntax-backward " ")
  (insert (format-time-string remember-planner-timestamp-format
                              (current-time)))
  (planner-update-note))

(defun remember-planner-add-xref (&optional plan-page)
  "Move the main text into PAGE.
Replace the day page entry with a cross-reference.
This should be called from the day page."
  (interactive (list (planner-read-name (planner-file-alist))))
  (or plan-page (setq plan-page (planner-read-name (planner-file-alist))))
  (when (string-match planner-date-regexp (planner-page-name))
    (save-restriction
      (planner-narrow-to-note)
      (goto-char (point-min))
      (when (looking-at "^.#\\([0-9]+\\)\\s-+\\(.*\\)")
        (let* (plan-number
               (day-number (planner-match-string-no-properties 1))
               (day-page (planner-today))
               (title (planner-match-string-no-properties 2))
               (planner-default-page (or remember-planner-page (planner-today)))
               (body (buffer-substring-no-properties (planner-line-end-position)
                                                     (point-max))))
          (unless (or (not plan-page) (equal plan-page (planner-today)))
	    (when (and (featurep 'planner-multi)
                       (string-match planner-multi-separator plan-page))
	      (setq plan-page (concat (planner-make-link (concat (planner-today) "#" day-number))
				      planner-multi-separator plan-page)))
	    (planner-replan-note plan-page)
            (goto-char (point-min))))))))

(defun remember-planner-set-default-page ()
  "Set `remember-planner-page' if called from a plan page."
  (setq remember-planner-page
        (and (equal major-mode 'planner-mode)
             (not (string-match planner-date-regexp
                                (planner-page-name)))
             (planner-page-name))))

(add-hook 'remember-before-remember-hook 'remember-planner-set-default-page)
(custom-add-option 'remember-handler-functions 'remember-planner-append)

(provide 'remember-planner)

;;; remember-planner.el ends here
