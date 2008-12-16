(setq emacs-wiki-ignored-extensions-regexp "\\.\\(bz2\\|gz\\|[Zz]\\|enc\\)\\'")
(setq emacs-wiki-charset-default "utf-8")

;; NOTE: certain system-specific definitions must appear in your your .emacs.
;;
;;(defvar emacs-wiki-maintainer "mailto:jimm@io.com")
;;(defvar emacs-wiki-publishing-directory "~/public_html/WebWiki")
;;  note can't use *my-pim-dir* below
;;(defvar emacs-wiki-projects
;;  '(("HomeWiki" . ((emacs-wiki-directories . ("~/pim/wiki))))
;;    ("OtherWiki" . ((emacs-wiki-directories . ("~/OtherDir"))))))
;;(defvar my-emacs-wiki-url-prefix "http://localhost/~jimm/WebWiki/")

(setq emacs-wiki-use-mode-flags nil)
(setq emacs-wiki-private-pages '("Secure$"))
(setq emacs-wiki-style-sheet
      "<link rel='stylesheet' type='text/css' href='../emacs-wiki.css'>")
(setq emacs-wiki-publishing-header
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">
<html>
  <head>
    <title><lisp>(emacs-wiki-page-title)</lisp></title>
    <meta name='generator' content='emacs-wiki.el'>
    <meta http-equiv='<lisp>emacs-wiki-meta-http-equiv</lisp>'
	  content='<lisp>emacs-wiki-meta-content</lisp>'>
    <link rev='made' href='<lisp>emacs-wiki-maintainer</lisp>'>
    <link rel='home' href='<lisp>(emacs-wiki-published-name
				     emacs-wiki-home-page)</lisp>'>
    <link rel='index' href='<lisp>(emacs-wiki-published-name
				      emacs-wiki-index-page)</lisp>'>
    <lisp>emacs-wiki-style-sheet</lisp>
  </head>
  <body>
    <lisp>(my-emacs-wiki-header)</lisp>
    <lisp>(my-emacs-wiki-page-top)</lisp>
    <div class='pagebody'>
    <!-- Page published by Emacs Wiki begins here -->\n"
)
(setq emacs-wiki-publishing-footer
      (concat "
</div>" emacs-wiki-publishing-footer))

(defun my-emacs-wiki-header ()
  "")
(defun my-emacs-wiki-home-icon ()
  "<a href='WelcomePage.html'><img src='../images/KeyMaster.gif'></a>")

(defun my-emacs-wiki-page-title ()
  (concat "<a href='../wiki_grep.cgi?search=" (emacs-wiki-page-name) "'>"
	  (emacs-wiki-page-title) "</a>")
  )

(defvar my-emacs-wiki-recent-changes-link
  "<a href='../wiki_recent.cgi'>Recent Changes</a>")
(defvar my-emacs-wiki-search-link
  "<a href='../wiki_search.html'>Search</a>")
(defun my-emacs-wiki-page-top ()
  (concat "<div class='navhead'><table width='100%'><tr><td align='left'><h1>"
	  (my-emacs-wiki-home-icon) " " (my-emacs-wiki-page-title)
	  "</h1></td><td align='right'>"
	  my-emacs-wiki-recent-changes-link
	  " "
	  my-emacs-wiki-search-link
	  "</td></tr></table></div>"))

;; emacs-wiki-visit-published-file doesn't work for me because I want to
;; view the page through Apache.
(defun my-wiki-view-page ()
  (interactive)
  (funcall emacs-wiki-browse-url-function
	   (concat my-emacs-wiki-url-prefix ; defined in .emacs
		   (emacs-wiki-published-name (emacs-wiki-page-name)))))

(defun my-wiki-insert-date (numeric-arg)
  "Inserts date stamp. When prefix argument numeric-arg is not 1,
inserts date and time stamp."
  (interactive "*p")
  (cond ((= 1 numeric-arg)
	 (insert (format-time-string "%Y-%m-%d")))
	(t (insert (format-time-string "%Y-%m-%d %H:%M:%S")))))

(defun my-wiki-insert-date-link (numeric-arg)
  "Inserts date stamp prefixed with 'date:'. When prefix argument numeric-arg
is not 1, inserts date and time stamp."
  (interactive "*p")
  (insert "date:")
  (my-wiki-insert-date numeric-arg))

(add-hook
 'emacs-wiki-mode-hook
 (lambda ()
   (define-key emacs-wiki-mode-map "\C-c\C-h" 'my-wiki-view-page)
   (define-key emacs-wiki-mode-map "\C-c=" my-shell)
   (define-key emacs-wiki-mode-map "\C-cd" 'my-wiki-insert-date)
   (define-key emacs-wiki-mode-map "\C-c\C-d" 'my-wiki-insert-date-link)))
