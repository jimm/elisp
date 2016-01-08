(load-file (concat *my-emacs-lib-dir* "bootstrap/mac-common.el"))

(defvar *my-erlang-emacs-tools-dir*
  (concat (car (file-expand-wildcards "/opt/local/lib/erlang/lib/tools*"))
          "/emacs/"))

(when window-system
  (defvar *basic-frame-alist* '((background-color . "gray95")
				(cursor-color . "orange")
				(tool-bar-mode . nil)))
  (setq initial-frame-alist
	(append *basic-frame-alist* '((height . 48) (top . 0) (left . 0))))
  (setq default-frame-alist *basic-frame-alist*)
  (set-face-attribute 'mode-line nil :foreground "yellow" :background "black"))

(defvar *my-erlang-emacs-tools-dir*
  (concat (car (reverse
                (sort (file-expand-wildcards "/opt/local/lib/erlang/lib/tools*") 'string-lessp)
               ))
          "/emacs/"))
