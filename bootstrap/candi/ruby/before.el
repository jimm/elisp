(load-file (concat *my-emacs-lib-dir* "bootstrap/mac-common.el"))

(when window-system
  (defvar *basic-frame-alist* '((background-color . "GhostWhite")
				(tool-bar-mode . nil)
                                (font . "Menlo 13")))
  (setq initial-frame-alist
	(append *basic-frame-alist* '((height . 52) (top . 0) (left . 0))))
  (setq default-frame-alist *basic-frame-alist*))

(defvar *my-erlang-emacs-tools-dir*
  (concat (car (file-expand-wildcards "/opt/local/lib/erlang/lib/tools*"))
          "/emacs/"))