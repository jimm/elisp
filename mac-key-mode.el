;;; mac-key-mode.el --- provide mac-style key bindings on Carbon Emacs

;; Copyright (C) 2004-2005  Seiji Zenitani <zenitani@mac.com>

;; Author: Seiji Zenitani <zenitani@mac.com>
;; Version: v20050518
;; Keywords: tools, mac
;; Created: 2004-12-27
;; Compatibility: Mac OS X (Carbon Emacs)
;; URL(jp): http://macwiki.sourceforge.jp/cgi-bin/wiki.cgi?mac-key-mode
;; URL(en): http://macemacsjp.sourceforge.jp/en/

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides mac-key-mode, a minor mode that provides
;; mac-like key bindings and relevant elisp functions.
;;
;; To use this package, add these lines to your .emacs file:
;;
;; ;;    (require 'redo)
;;     (require 'mac-key-mode)
;;     (mac-key-mode 1)
;;
;; Note that mac-key-mode requires redo.el and that it turns on
;; pc-selection-mode.
;; In order to set additional key bindings,
;; modify mac-key-mode-map in your .emacs file:
;;
;;     (require 'mac-key-mode)
;;     (define-key mac-key-mode-map [(alt l)] 'goto-line)
;;


;;; Code:

;; requires redo
(require 'redo)

(defgroup mac-key-mode nil
  "Mac-style key-binding mode."
  :group 'mac
  :version "22.1")

(defvar mac-key-mode-backup-command-key-is-meta nil
  "Do not use this variable.")
(defvar mac-key-mode-backup-pc-selection-mode nil
  "Do not use this variable.")
(defcustom mac-key-mode-modify-file-menu-flag t
  "If non-nil, `mac-key-mode' addes two menu items to the File menu
in the menu bar."
  :group 'mac-key-mode
  :type 'boolean)
(defcustom mac-key-mode-lighter
  (concat " " (char-to-string 323935)) ;; the Apple mark
;;  (concat " " (char-to-string (ucs-to-char 63743))) ;; the Apple mark
  "A lighter string which is displayed in the modeline
when `mac-key-mode' is on."
  :group 'mac-key-mode
  :type 'string)

(defvar mac-key-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(alt c)] 'kill-ring-save)
    (define-key map [(alt v)] 'yank)
    (define-key map [(alt x)] 'kill-region)
    (define-key map [(alt a)] 'mark-whole-buffer)
; jimm
;    (define-key map [(alt z)] 'undo)
;    (define-key map [(alt meta z)] 'redo) ; requires redo
    (define-key map [(alt f)] 'isearch-forward)
    (define-key map [(alt meta f)] 'occur)
    (define-key map [(alt g)] 'isearch-repeat-forward)
    (define-key map [(alt o)] 'mac-key-open-file)
    (define-key map [(alt s)] 'save-buffer)
    (define-key map [(alt w)] 'kill-this-buffer)
    (define-key map [(alt m)] 'iconify-frame)
    (define-key map [(alt q)] 'save-buffers-kill-emacs)
    (define-key map [(alt \`)] 'other-frame)
    (define-key map [(alt p)] 'ps-print-buffer-with-faces)
    (define-key map [(alt i)] 'mac-key-show-in-finder)
    (define-key map [(alt t)] 'mac-key-open-terminal)
    (define-key map [(alt .)] 'keyboard-quit)
    (define-key map [(alt up)] 'beginning-of-buffer)
    (define-key map [(alt down)] 'end-of-buffer)
    (define-key map [(alt left)] 'beginning-of-line)
    (define-key map [(alt right)] 'end-of-line)
    (define-key map [(alt \ )] 'toggle-input-method)
    (define-key map [A-mouse-1] 'browse-url-at-mouse) ; command + click
    map)
  "Keymap for `mac-key-mode'.")

(define-minor-mode mac-key-mode
  "Toggle Mac Key mode.
With arg, turn Mac Key mode on if arg is positive.
When Mac Key mode is enabled, mac-style key bindings are provided."
  :global t
  :group 'mac-key-mode
  :lighter mac-key-mode-lighter
  :keymap 'mac-key-mode-map
  (if mac-key-mode
      (progn

        (setq mac-key-mode-backup-command-key-is-meta mac-command-key-is-meta
              mac-key-mode-backup-pc-selection-mode pc-selection-mode)
        (setq mac-command-key-is-meta nil)
        (pc-selection-mode t)
        (when mac-key-mode-modify-file-menu-flag
          (define-key-after menu-bar-file-menu [my-file-separator]
            '("--" . nil) 'recover-session)
          (define-key-after menu-bar-file-menu [mac-show-in-finder]
            '("Show In Finder" . mac-key-show-in-finder) 'my-file-separator)
          (define-key-after menu-bar-file-menu [mac-open-terminal]
            '("Open Terminal" . mac-key-open-terminal) 'mac-show-in-finder)
          )
        )
    (progn

      (setq mac-command-key-is-meta mac-key-mode-backup-command-key-is-meta)
      (pc-selection-mode mac-key-mode-backup-pc-selection-mode)
      (define-key global-map [menu-bar file my-file-separator] nil)
      (define-key global-map [menu-bar file mac-show-in-finder] nil)
      (define-key global-map [menu-bar file mac-open-terminal] nil)

      ))
  )


;; courtesy of Lawrence Akka (EmacsWiki: MacOSTweaks)
(defun mac-key-open-file (filename &optional wildcards)
  "Open a file using standard file open dialog."
  (interactive
   (let ((last-nonmenu-event nil)) 
     (find-file-read-args "Find existing file: " t)))
  (find-file-existing filename wildcards)
  )

;; (defun mac-key-open-file ()
;;   "Document forthcoming..."
;;   (interactive)
;;   (let ((file (do-applescript "try
;; POSIX path of (choose file)
;; end try")))
;;     (if (> (length file) 3)
;;         (if (equal current-language-environment "Japanese")
;;             (setq file
;;                   (replace-regexp-in-string
;;                    "\\\\\\(.\\)" "\\1"
;;                    (decode-coding-string
;;                     (substring file 1 (- (length file) 1))
;;                     'sjis-mac)))
;;           (setq file
;;                 (substring file 1 (- (length file) 1)))
;;           ))
;;     (if (and (not (equal file ""))(file-readable-p file))
;;         (find-file file)
;;       (beep))
;;     ))


;; Show In Finder

(defun mac-key-show-in-finder ()
  "Document forthcoming..."
  (interactive)
  (if (stringp (buffer-file-name))
      (do-applescript
       (format "
tell application \"Finder\"
  activate
  try
    select file \"%s\"
  on error
    beep
  end try
end tell"
               (if (eq selection-coding-system 'sjis-mac)
                   (replace-regexp-in-string
                    "\\\\" "\\\\\\\\"
                    (encode-coding-string
                     (posix-file-name-to-mac (buffer-file-name))
                     selection-coding-system))
                 (encode-coding-string
                  (posix-file-name-to-mac (buffer-file-name))
                  selection-coding-system))
               ))
    (shell-command "/usr/bin/open .")
    ))


;; Open Terminal.app

(defun mac-key-open-terminal ()
  "Document forthcoming..."
  (interactive)
  (let ((dir ""))
    (cond
     ((and (local-variable-p 'dired-directory) dired-directory)
      (setq dir dired-directory))
     ((stringp (buffer-file-name))
      (setq dir (file-name-directory (buffer-file-name))))
     )
    (do-applescript
     (format "
tell application \"Terminal\"
  activate
  try
    do script with command \"cd %s\"
  on error
    beep
  end try
end tell" dir))
    ))


(provide 'mac-key-mode)

;;; mac-key-mode.el ends here.
