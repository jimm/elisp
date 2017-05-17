(load-file "~/.emacs.d/elisp/bootstrap-init.el")
(bootstrap-init "home" "sysex")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet smex projectile-rails markdown-mode magit haskell-mode go-mode fzf dumb-jump deft coffee-mode alchemist ag ace-window 2048-game))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-prompt ((((class color) (background light)) (:foreground "Blue")) (((class color) (background dark)) (:foreground "SteelBlue")) (t (:bold t)))))
