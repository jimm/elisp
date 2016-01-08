(load-file (concat *my-emacs-lib-dir* "bootstrap/mac-common.el"))

(when window-system
  (defvar *basic-frame-alist* '((background-color . "ghost white")
                                (cursor-color . "orange")
				(tool-bar-mode . nil)
                                (font . "Menlo 13")))
  (setq initial-frame-alist
	(append *basic-frame-alist* '((height . 60) (top . 0) (left . 0))))
  (setq default-frame-alist *basic-frame-alist*)
  (set-face-attribute 'mode-line nil :foreground "yellow" :background "black"))

(defvar *my-erlang-emacs-tools-dir*
  (concat (car (file-expand-wildcards "/opt/local/lib/erlang/lib/tools*"))
          "/emacs/"))
