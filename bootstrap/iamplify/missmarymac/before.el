(defvar emacs-wiki-maintainer "mailto:jim@iamplify.com")
(defvar emacs-wiki-publishing-directory "~/Sites/WebWiki")
(defvar my-emacs-wiki-url-prefix "http://localhost/~jimm/WebWiki/")
(defvar emacs-wiki-projects
  '(("IampWiki" . ((emacs-wiki-directories . ("~/Wiki"))))
    ("HomeWiki" . ((emacs-wiki-directories . ("~/src/jimm/pim/wiki"))))))

(when window-system
  (defvar *basic-frame-alist* '((background-color . "gray90")
				(tool-bar-lines . 0)
				(width . 80)))
  (setq initial-frame-alist
	(append *basic-frame-alist* '((height . 53) (top . 0) (left . 0))))
  (setq default-frame-alist *basic-frame-alist*))

(add-to-list 'load-path "/usr/share/emacs/21.2/lisp" t)
(add-to-list 'load-path "/usr/share/emacs/21.2/site-lisp" t)
(defvar *my-erlang-emacs-tools-dir*
  "/usr/local/src/Erlang/current/lib/tools/emacs")
