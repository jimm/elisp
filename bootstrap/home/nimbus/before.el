(load-file (concat *my-emacs-lib-dir* "bootstrap/mac-common.el"))

(when window-system
  (defvar *basic-frame-alist* '(; (background-color . "lavender")
                                ; (cursor-color . "green")
				(tool-bar-mode . nil)))
  (setq initial-frame-alist
	(append *basic-frame-alist* '((height . 76) (top . 0) (left . 0))))
  (setq default-frame-alist *basic-frame-alist*))

(add-to-list 'load-path "/usr/share/emacs/22.1/lisp" t)
(add-to-list 'load-path "/usr/share/emacs/22.1/site-lisp" t)
(defvar *my-erlang-emacs-tools-dir*
  (concat (car (file-expand-wildcards "/opt/local/lib/erlang/lib/tools*"))
          "/emacs/"))
