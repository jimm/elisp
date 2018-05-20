(defvar *my-pim-dir* "~/pim/")

(when window-system
  (let ((basic-frame-alist '((background-color . "white")
                             (tool-bar-mode . nil))))
    (setq initial-frame-alist
          (append basic-frame-alist '((width . 155) (height . 42)
                                      (top . 0) (left . 0)))
          default-frame-alist basic-frame-alist)))
