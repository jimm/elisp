(defvar *my-pim-dir* "/Users/jimm/pim/")

(setq ns-command-modifier 'meta)        ; define Command as Meta key
(setq ns-option-modifier "none")        ; unbind option key

; Smoother mouse wheel scrolling
; http://www.reddit.com/r/programming/comments/95uv7/emacs_231_has_been_released/c0bj6zp
(setq mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))

(when window-system
  (defvar *basic-frame-alist* '((tool-bar-mode . nil)))
  (setq initial-frame-alist
	(append *basic-frame-alist* '((height . 77) (top . 0) (left . 0)
                                      (background-color . "ghost white")
                                      (cursor-color . "orange"))))
  (setq default-frame-alist initial-frame-alist)
  (set-face-attribute 'mode-line nil :foreground "yellow" :background "black"))

(add-to-list 'load-path "/usr/share/emacs/22.1/lisp" t)
(add-to-list 'load-path "/usr/share/emacs/22.1/site-lisp" t)

(defvar *my-erlang-emacs-tools-dir*
  (concat (car (reverse
                (sort (file-expand-wildcards "/usr/local/lib/erlang/lib/tools*") 'string-lessp)
               ))
          "/emacs/"))

(setq-default indent-tabs-mode nil)
