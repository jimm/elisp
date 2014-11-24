;;; See http://sonic-pi.net/ and
;;; https://github.com/repl-electric/sonic-pi.el

(add-to-list 'load-path "~/.emacs.d/sonic-pi/")
(require 'sonic-pi)
(add-hook 'sonic-pi-mode-hook
          (lambda ()
            (setq sonic-pi-path "~/src/sonic-pi/")
            (define-key ruby-mode-map "\C-c\C-b" 'sonic-pi-stop-all)))
