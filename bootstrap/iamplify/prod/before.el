(defvar emacs-wiki-maintainer "mailto:jim@iamplify.com")
(defvar emacs-wiki-publishing-directory "~/Sites/WebWiki")
(defvar my-emacs-wiki-url-prefix "http://localhost/~jimm/WebWiki/")
(defvar emacs-wiki-projects
  '(("IampWiki" . ((emacs-wiki-directories . ("~/Wiki"))))
    ("HomeWiki" . ((emacs-wiki-directories . ("~/src/jimm/pim/wiki"))))))

(when window-system
  (defvar *basic-frame-alist* '((background-color . "gray90")
				(tool-bar-mode . nil)))
  (setq initial-frame-alist
	(append *basic-frame-alist* '((height . 53) (top . 0) (left . 0))))
  (setq default-frame-alist *basic-frame-alist*))

(add-to-list 'load-path "/usr/share/emacs/21.3/lisp" t)
(add-to-list 'load-path "/usr/share/emacs/21.3/site-lisp" t)
(defvar *my-erlang-emacs-tools-dir*
  "/usr/local/src/Erlang/current/lib/tools/emacs")

(defvar *my-sql-regex*
  "\\b\\\(backoffice\\|osc\\|papro\\|financial\\|postoffice\\|mediafactory\\)\\."
  "See my-sql-send-paragraph.")
(defvar *my-sql-regex-replacement* "\\1."
  "See my-sql-send-paragraph.")
