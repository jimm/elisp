(defvar emacs-wiki-maintainer "mailto:jimm@io.com")
(defvar emacs-wiki-publishing-directory "~/public_html/WebWiki")
(defvar my-emacs-wiki-url-prefix "http://jimm.textdriven.com/WebWiki/")
(defvar emacs-wiki-projects
  '(("default" . ((emacs-wiki-directories . ("~/pim/wiki"))))))
(defvar *my-erlang-emacs-tools-dir*
  (concat
   (car (file-expand-wildcards "/usr/local/src/Erlang/current/lib/tools*"))
   "/emacs/"))
