(defvar *my-pim-dir* "~/pim/")
(defvar *my-erlang-emacs-tools-dir*
  (concat
   (car (file-expand-wildcards "/usr/local/src/Erlang/current/lib/tools*"))
   "/emacs/"))
(add-to-list 'load-path "~/.emacs.d/org/lisp" t)
(add-to-list 'load-path "~/.emacs.d/org/contrib/lisp" t)
