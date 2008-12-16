(defvar emacs-wiki-maintainer "mailto:jimm@io.com")
(defvar emacs-wiki-publishing-directory "/Library/WebServer/Documents/WebWiki")
(defvar my-emacs-wiki-url-prefix "http://localhost/WebWiki/")
(defvar emacs-wiki-projects
  '(("HomeWiki" . ((emacs-wiki-directories . ("~/src/jimm/pim/wiki"))))
    ("IampWiki" . ((emacs-wiki-directories . ("~/biz/iAmplify/Wiki"))))))

(defvar *my-erlang-emacs-tools-dir*
  "/usr/local/src/Erlang/current/lib/tools/emacs")

(when window-system
  (defvar *basic-frame-alist* '((background-color . "gray90")
				(tool-bar-mode . nil)))
  (setq initial-frame-alist
	(append *basic-frame-alist* '((height . 48) (top . 0) (left . 0))))
  (setq default-frame-alist *basic-frame-alist*))

(add-to-list 'load-path "/usr/share/emacs/21.2/lisp" t)
(add-to-list 'load-path "/usr/share/emacs/21.2/site-lisp" t)
