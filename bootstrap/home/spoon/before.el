(defvar *my-pim-dir* "/Users/jimm/pim/")
(defvar *arc-dir* "/usr/local/src/Lisp/arc/")

(setq ns-command-modifier 'meta)        ; define Command as Meta key
(setq ns-option-modifier "none")        ; unbind option key

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

(add-to-list 'load-path "/usr/share/emacs/21.2/lisp" t)
(add-to-list 'load-path "/usr/share/emacs/21.2/site-lisp" t)
(defvar *my-erlang-emacs-tools-dir*
  (concat (car (reverse
                (sort (file-expand-wildcards "/opt/local/lib/erlang/lib/tools*") 'string-lessp)
               ))
          "/emacs/"))

(setq-default indent-tabs-mode nil)
