;; -*- emacs-lisp -*-
(defvar *my-emacs-lib-dir* "~/Library/elisp/")
(load-file (concat *my-emacs-lib-dir* "bootstrap-init.el"))
(bootstrap-init "home" "spoon")
