(defvar *my-pim-dir* "~/pim/")
(defvar *scala-emacs-support-dir* "~/src/scala-emacs/")

(setq ns-command-modifier 'meta)        ; define Command as Meta key
(setq ns-option-modifier "none")        ; unbind option key

; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=5683#19
(when (fboundp 'ns-list-colors)
  (setq x-colors (ns-list-colors)))     ; fix build bug in 23.4, also in 24.1

; Smoother mouse wheel scrolling
; http://www.reddit.com/r/programming/comments/95uv7/emacs_231_has_been_released/c0bj6zp
(setq mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))

(when window-system
  (defvar *basic-frame-alist* '((background-color . "ghost white")
                                (cursor-color . "orange")
				(tool-bar-mode . nil)
                                (font . "-apple-Monaco-medium-normal-normal-*-14-*-*-*-m-0-fontset-auto1")))
  (setq initial-frame-alist
	(append *basic-frame-alist* '((height . 76) (top . 0) (left . 0))))
  (setq default-frame-alist *basic-frame-alist*)
  (set-face-attribute 'mode-line nil :foreground "yellow" :background "black"))

(add-to-list 'load-path "/usr/share/emacs/22.1/lisp" t)
(add-to-list 'load-path "/usr/share/emacs/22.1/site-lisp" t)
(defvar *my-erlang-emacs-tools-dir*
  (concat (car (file-expand-wildcards "/opt/local/lib/erlang/lib/tools*"))
          "/emacs/"))

(setq-default indent-tabs-mode nil)
