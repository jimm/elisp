(defun my-emacs-wiki-home-icon ()
  "<a href='WelcomePage.html'><img src='../images/logo.gif'></a>")
(setq emacs-wiki-private-pages (cons "SharedSecrets" emacs-wiki-private-pages))

(defun my-visit-home-todo () (interactive)
  (find-file (concat *my-pim-dir* "wiki/ToDo")))
(defun my-visit-notes-page () (interactive) (emacs-wiki-find-file "NotesPage"))
(defun my-visit-foo-sql () (interactive) (find-file "/tmp/foo.sql"))

(defun my-run-framework-testcase ()
  (interactive)
  (compile (concat "ant -e -s build.xml -Dtestclass="
		   (path-to-java-package (buffer-file-name)) "."
		   (file-name-sans-extension (file-name-nondirectory
					      (buffer-file-name)))
		   " test-one")))

(define-key global-map [\C-f4] 'my-visit-home-todo)
(define-key global-map [f6] 'my-visit-notes-page)
(define-key global-map [\C-f6] 'my-visit-foo-sql)
(add-hook 'java-mode-hook
	  '(lambda ()
	     (define-key java-mode-map "\C-cr" 'my-run-framework-testcase)
	     (define-key java-mode-map "\C-c\C-r" 'my-run-framework-testcase)))

(setq sql-user "viget")
(setq sql-server "db1.local")
(setq sql-database "backoffice")

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(setq compile-command "ant -e -s build.xml ")
