(when window-system
  (let ((basic-frame-alist '((background-color . "white")
                             (tool-bar-mode . nil)
                             (font . "Menlo 14"))))
    (setq initial-frame-alist
          (append basic-frame-alist '((width . 205) (height . 71)
                                      (top . 0) (left . 0)))
          default-frame-alist basic-frame-alist)))

(defvar *my-erlang-emacs-tools-dir*
  (concat (car (file-expand-wildcards "/opt/local/lib/erlang/lib/tools-*"))
          "/emacs/"))
