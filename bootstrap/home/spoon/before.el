(defvar *my-erlang-emacs-tools-dir*
  (concat (car (file-expand-wildcards "/opt/local/lib/erlang/lib/tools*"))
          "/emacs/"))

(when window-system
  (let ((basic-frame-alist '((background-color . "WhiteSmoke")
                             (tool-bar-mode . nil)
                             (font . "Menlo 13"))))
    (setq initial-frame-alist
          (append basic-frame-alist '((height . 48) (top . 0) (left . 0)))
          default-frame-alist basic-frame-alist)))

(defvar *my-erlang-emacs-tools-dir*
  (concat (car (file-expand-wildcards "/opt/local/lib/erlang/lib/tools-*"))
          "/emacs/"))
