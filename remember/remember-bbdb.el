;;; remember-bbdb --- BBDB support for remember.el

;; Copyright (C) 1999, 2000, 2001 John Wiegley
;; Copyright (C) 2003 Sandra Jean Chua

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Sacha Chua <sacha@free.net.ph>
;; Created: 29 Mar 1999
;; Keywords: data memory todo pim bbdb
;; URL: http://gna.org/projects/remember-el/

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

;; This adds completing reads to remember-bbdb-store-in-mailbox. To
;; use, add (require 'remember-bbdb) to your .emacs, and make your
;; remember-handler-functions include remember-bbdb-store-in-mailbox.

;;; Code:

(require 'remember)
(require 'bbdb-com)

;;;###autoload
(defun remember-bbdb-store-in-mailbox ()
  "Store remember data as if it were incoming mail.
In which case `remember-mailbox' should be the name of the mailbox.
Each piece of psuedo-mail created will have an `X-Todo-Priority'
field, for the purpose of appropriate splitting."
  (let ((who (bbdb-completing-read-record
              "Who is this item related to? "))
        (moment
         (format "%.0f" (remember-time-to-seconds (current-time))))
        (desc (remember-buffer-desc))
        (text (buffer-string))
        name address)
    (with-temp-buffer
      (if (vectorp who)
          (let ((net (bbdb-record-net who)))
            (setq name (bbdb-record-name who)
                  address (if (listp net)
                              (car net)
                            net)))
        (if (stringp who)
            (setq name who)
          (setq name (user-full-name)
                address user-mail-address)))
      (insert (format "
From %s  %s
Date: %s
From: %s
Message-Id: <remember-%s@%s>
X-Todo-Priority: %s
To: %s <%s>
Subject: %s\n\n"
                      (user-login-name)
                      (remember-mail-date)
                      (remember-mail-date t)
                      (if (and name address)
                          (format "%s <%s>" name address)
                        (or name address))
                      moment (system-name)
                      remember-default-priority
                      (user-full-name) user-mail-address
                      desc))
      (let ((here (point)))
        (insert text)
        (unless (bolp)
          (insert "\n"))
        (insert "\n")
        (goto-char here)
        (while (re-search-forward "^\\(From[: ]\\)" nil t)
          (replace-match ">\\1")))
      (append-to-file (point-min) (point-max) remember-mailbox)
      t)))

(custom-add-option 'remember-handler-functions 'remember-bbdb-store-in-mailbox)

(provide 'remember-bbdb)

;;; remember-bbdb.el ends here
