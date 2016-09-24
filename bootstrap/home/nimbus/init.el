;; -*- emacs-lisp -*-
(defvar *my-emacs-lib-dir* "~/.emacs.d/elisp/")
(load-file (concat *my-emacs-lib-dir* "bootstrap-init.el"))
(bootstrap-init "home" "nimbus")
(when (fboundp #'package-initialize)
  (package-initialize))
(bootstrap-load)
