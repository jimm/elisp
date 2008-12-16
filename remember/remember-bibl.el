;;; remember-bibl --- Bibl support for remember.el

;; Copyright (C) 1999, 2000, 2001 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Sacha Chua <sacha@free.net.ph>
;; Created: 29 Mar 1999
;; Keywords: data memory todo pim bibliography
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

;; You can get bibl-mode from
;; http://ftp.azc.uam.mx/mirrors/gnu/emacs-lisp/bosullivan-packages/bibl-mode/

;;; Code:

(require 'url)
(require 'bibl-mode)
(require 'bookmark)

;;;###autoload
(defun remember-url ()
  "Remember a URL in `bibl-mode' that is being visited with w3."
  (interactive)
  (require 'bibl-mode)
  (save-excursion
    (let ((url (url-view-url t))
          (name (buffer-name))
          (keywords (read-string "Keywords: "))
          (desc (read-string "Description: ")))
      (bibl-visit-create)
      (insert name)
      (forward-line 1) (end-of-line)
      (insert keywords)
      (forward-line 1) (end-of-line)
      (insert desc)
      (forward-line 1) (end-of-line)
      (insert url)
      (bibl-tidy-record)
      (save-buffer)
      (bury-buffer))))

;;;###autoload
(defun remember-location ()
  "Remember a bookmark location in `bibl-mode'."
  (interactive)
  (require 'bookmark)
  (require 'bibl-mode)
  (save-excursion
    (let ((name (read-string "Title: "))
          (keywords (read-string "Keywords: "))
          (desc (read-string "Description: ")))
      (bookmark-set name)
      (bookmark-set-annotation name desc)
      (bibl-visit-create)
      (insert name)
      (forward-line 1) (end-of-line)
      (insert keywords)
      (forward-line 1) (end-of-line)
      (insert desc)
      (forward-line 1) (end-of-line)
      (insert (bookmark-get-filename name))
      (bibl-tidy-record)
      (bookmark-delete name)
      (when remember-save-after-remembering (save-buffer))
      (bury-buffer))))



(provide 'remember-bibl)

;;; remember-bibl.el ends here
