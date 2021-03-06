(defvar *my-pim-dir* "~/pim/")

(setq shell-file-name "/bin/bash"
      epa-pinentry-mode 'loopback)

(when window-system
  (let ((basic-frame-alist '((tool-bar-mode . nil)
                             (font . "Menlo 13"))))
    (setq initial-frame-alist
          (append basic-frame-alist '((width . 176) (height . 56)
                                      (top . 0) (left . 0)))
          default-frame-alist basic-frame-alist)))
