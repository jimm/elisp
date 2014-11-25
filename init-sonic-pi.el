;;; See http://sonic-pi.net/ and
;;; https://github.com/repl-electric/sonic-pi.el

;; M-x sonic-pi-mode
;; M-x sonic-pi-jack-in
;;     or, if already running sonic-pi-server
;; M-x sonic-pi-connect

;; To send buffer to sonic-pi, use C-c C-k
;; To stop play, use C-c C-b

(add-to-list 'load-path "~/.emacs.d/sonic-pi/")
(require 'sonic-pi)
(add-hook 'sonic-pi-mode-hook
          (lambda ()
            (setq sonic-pi-path "~/src/sonic-pi/")
            (define-key ruby-mode-map "\C-c\C-b" 'sonic-pi-stop-all)))
