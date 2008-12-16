(add-to-list 'load-path ".")

;; If you want Remember to find all of these programs when
;; byte-compiling Remember, make sure to edit these paths to match your
;; configuration.

; jimm
; (add-to-list 'load-path (concat *my-emacs-lib-dir* "muse/lisp/") t)
; (add-to-list 'load-path (concat *my-emacs-lib-dir* "planner/") t)
; (add-to-list 'load-path (concat *my-emacs-lib-dir* "bbdb/lisp/") t)

(defun remember-elint-files ()
  (require 'elint)
  (elint-initialize)

  (defvar nomessage t)
  (load "remember" nil nomessage)
  (dolist (file (directory-files "." nil "\\.el$"))
    (setq file (substring file 0 (string-match "\\.el$" file)))
    (load file nil nomessage))

  (add-to-list 'elint-standard-variables 'current-prefix-arg)
  (add-to-list 'elint-standard-variables 'command-line-args-left)
  (add-to-list 'elint-standard-variables 'buffer-file-coding-system)
  (add-to-list 'elint-standard-variables 'save-some-buffers-action-alist)
  (add-to-list 'elint-standard-variables 'emacs-major-version)
  (add-to-list 'elint-standard-variables 'emacs-minor-version)
  (add-to-list 'elint-standard-variables 'emacs-version)
  (add-to-list 'elint-standard-variables 'window-system)
  (add-to-list 'elint-standard-variables 'debug-on-error)

  (dolist (file command-line-args-left)
    (find-file file)
    (message "Checking %s ..." file)
    (elint-current-buffer)
    (with-current-buffer (elint-get-log-buffer)
      (goto-char (point-min))
      (forward-line 2)
      (while (not (or (eobp)
                      (looking-at "^Linting complete")))
        (message (buffer-substring (point-at-bol)
                                   (point-at-eol)))
        (forward-line 1)))
    (kill-buffer (current-buffer))))

(defun remember-generate-autoloads ()
  (interactive)
  (defvar autoload-package-name)
  (defvar command-line-args-left)
  (defvar generated-autoload-file)
  (require 'autoload)
  (setq generated-autoload-file (expand-file-name "remember-autoloads.el"))
  (setq command-line-args-left (mapcar #'expand-file-name
                                       command-line-args-left))
  (if (featurep 'xemacs)
      (progn
          (setq autoload-package-name "remember")
          (batch-update-autoloads))
    (find-file generated-autoload-file)
    (delete-region (point-min) (point-max))
    (insert ";;; remember-autoloads.el --- autoloads for Remember
;;
;;; Code:
")
    (save-buffer 0)
    (batch-update-autoloads)
    (find-file generated-autoload-file)
    (goto-char (point-max))
    (insert ?\n)
    (insert "(provide 'remember-autoloads)
;;; remember-autoloads.el ends here
;;
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
")
    (insert ?\n)
    (save-buffer 0)
    (kill-buffer (current-buffer))))
