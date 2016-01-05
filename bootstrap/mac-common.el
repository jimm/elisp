;; For each PATH element returned by launchctl, add it to exec-path if it's
;; not already there. Also make sure it's set as an env var properly within
;; Emacs.
(let ((true-path (shell-command-to-string "launchctl getenv \"PATH\"")))
  (mapc (lambda (path)
          (add-to-list 'exec-path path))
        (split-string true-path ":"))
  (setenv "PATH" true-path))

(defvar *my-pim-dir* "~/pim/")

(setq my-shell #'eshell)
(setq my-alternate-shell #'shell)

(setq ns-command-modifier 'meta)        ; define Command as Meta key
(setq ns-option-modifier 'super)        ; define Option as Super key

; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=5683#19
(when (fboundp 'ns-list-colors)
  (setq x-colors (ns-list-colors)))     ; fix build bug in 23.4, also in 24.1

; Smoother mouse wheel scrolling
; http://www.reddit.com/r/programming/comments/95uv7/emacs_231_has_been_released/c0bj6zp
(setq mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))
