(load-file (concat *my-emacs-lib-dir* "bootstrap/work-common.el"))

(setq shell-file-name "/bin/bash"
      epa-pinentry-mode 'loopback)

(when window-system
  (let ((basic-frame-alist '((tool-bar-mode . nil)
                             (font . "Menlo 13"))))
    (set-face-attribute 'default t :font "Menlo 13")
    (setq
     initial-frame-alist basic-frame-alist
     default-frame-alist basic-frame-alist)))

(defvar *my-erlang-emacs-tools-dir*
  (concat (car (file-expand-wildcards "/usr/local/lib/erlang/lib/tools-*"))
          "/emacs/"))
