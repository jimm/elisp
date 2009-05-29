(when window-system
  (defvar *basic-frame-alist* '(; (background-color . "gray90")
				(tool-bar-mode . nil)))
  (setq initial-frame-alist
	(append *basic-frame-alist* '((height . 72) (top . 0) (left . 0))))
  (setq default-frame-alist *basic-frame-alist*))

(add-to-list 'load-path "/usr/share/emacs/22.1/lisp" t)
(add-to-list 'load-path "/usr/share/emacs/22.1/site-lisp" t)
(defvar *my-erlang-emacs-tools-dir*
  "/opt/local/lib/erlang/lib/tools-2.6.4/emacs/")

(setq-default indent-tabs-mode nil)
