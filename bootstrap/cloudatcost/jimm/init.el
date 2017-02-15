;; -*- emacs-lisp -*-
(defvar *my-emacs-lib-dir* "~/.emacs.d/elisp/")
(load-file (concat *my-emacs-lib-dir* "bootstrap-init.el"))
(bootstrap-init "cloudatcost" "jimm")

(custom-set-variables
 '(abbrev-mode nil)                     ; this is the default default, anyway
 '(android-mode-sdk-dir "/usr/local/android-sdk-mac")
 '(css-indent-offset 2)
 '(eshell-save-history-on-exit t)
 '(package-selected-packages
   (quote
    (yasnippet yaml-mode wttrin smex sicp sass-mode rspec-mode org-present markdown-mode magit less-css-mode inf-ruby inf-clojure http-twiddle haskell-mode hamlet-mode go-mode fzf flx-ido emms dumb-jump diminish deft coffee-mode bind-key alchemist ace-window 2048-game)))
 '(sgml-xml-mode t)
 '(woman-use-own-frame nil)
 '(sgml-xml-mode t))

(custom-set-faces
 '(aw-leading-char-face ((((class color)) (:foreground "red" :bold t))))
 '(eshell-prompt ((((class color) (background light)) (:foreground "Blue")) (((class color) (background dark)) (:foreground "SteelBlue")) (t (:bold t)))))
