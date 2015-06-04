;; For each PATH element returned by launchctl, add it to exec-path if it's
;; not already there. Also make sure it's set as an env var properly within
;; Emacs.
(let ((true-path (shell-command-to-string "launchctl getenv \"PATH\"")))
  (mapc (lambda (path)
          (add-to-list 'exec-path path))
        (split-string true-path ":"))
  (setenv "PATH" true-path))

(defvar *my-pim-dir* "~/pim/")

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
                                (font . "Menlo 13")))
  (setq initial-frame-alist
	(append *basic-frame-alist* '((height . 52) (top . 0) (left . 0))))
  (setq default-frame-alist *basic-frame-alist*)
  (set-face-attribute 'mode-line nil :foreground "yellow" :background "black"))

(defvar *my-erlang-emacs-tools-dir*
  (concat (car (file-expand-wildcards "/opt/local/lib/erlang/lib/tools*"))
          "/emacs/"))

(setq-default indent-tabs-mode nil)
