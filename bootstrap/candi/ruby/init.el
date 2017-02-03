;; -*- emacs-lisp -*-
(defvar *my-emacs-lib-dir* "~/.emacs.d/elisp/")
(load-file (concat *my-emacs-lib-dir* "bootstrap-init.el"))
(bootstrap-init "candi" "ruby")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rust-mode elm-mode elm-yasnippets yasnippet yaml-mode textile-mode smex sicp sass-mode rspec-mode projectile-rails ponylang-mode org-present markdown-mode magit lua-mode less-css-mode inf-clojure http-twiddle haskell-mode hamlet-mode go-mode fzf flx-ido emms dumb-jump diminish deft coffee-mode bind-key alchemist ag ace-window 2048-game))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
