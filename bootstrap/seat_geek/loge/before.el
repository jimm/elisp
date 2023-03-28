(load-file (concat *my-emacs-lib-dir* "bootstrap/work-common.el"))

(when window-system
  (let ((basic-frame-alist '((tool-bar-mode . nil)
                             (font . "Menlo 13")
                             (fullscreen . fullheight))))
    (set-face-attribute 'default t :font "Menlo 13")
    (setq
     initial-frame-alist basic-frame-alist
     default-frame-alist basic-frame-alist)))
