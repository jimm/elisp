(load-file (concat *my-emacs-lib-dir* "bootstrap/mac-common.el"))

(when window-system
  (let ((basic-frame-alist '((background-color . "WhiteSmoke")
                             (tool-bar-mode . nil)
                             (font . "Menlo 13"))))
    (setq initial-frame-alist
          (append basic-frame-alist '((width . 176) (height . 56)
                                      (top . 0) (left . 0)))
          default-frame-alist basic-frame-alist)))

(defvar *my-erlang-emacs-tools-dir*
  (concat (car (file-expand-wildcards "/opt/local/lib/erlang/lib/tools-*"))
          "/emacs/"))
