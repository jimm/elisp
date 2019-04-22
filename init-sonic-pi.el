;;; See http://sonic-pi.net/ and
;;; https://github.com/repl-electric/sonic-pi.el

;; M-x sonic-pi-mode
;; M-x sonic-pi-jack-in
;;     or, if already running sonic-pi-server
;; M-x sonic-pi-connect

;; To send buffer to sonic-pi, use C-c C-k
;; To stop play, use C-c C-b

(add-to-list 'load-path "~/.emacs.d/sonic-pi.el/")
(require 'sonic-pi)
(setq sonic-pi-path "~/src/miscellaneous/sonic-pi/") ; Must end with "/"

(defun sonic-pi-server-cmd ()
  "This overrides the definition already loaded, because the
command doesn't live where the existing code thinks it is."
  (concat sonic-pi-path "app/server/ruby/bin/sonic-pi-server.rb"))

;; Optionally define a hook
(add-hook 'sonic-pi-mode-hook
          (lambda ()
            (define-key ruby-mode-map "\C-c\C-b" 'sonic-pi-stop-all)))
