(defun my-emacs-wiki-home-icon ()
  "<a href='WelcomePage.html'><img src='../images/logo.gif'></a>")
(setq emacs-wiki-private-pages (cons "SharedSecrets" emacs-wiki-private-pages))

(defun my-visit-work-todo () (interactive) (find-file "~/Wiki/ToDo"))
(defun my-visit-home-todo () (interactive) (find-file (concat *my-pim-dir* "orgs/todo.org")))
(defun my-visit-home-notes-page () (interactive) (find-file *my-remember-data-file*))
(defun my-visit-work-notes-page () (interactive) (emacs-wiki-find-file "NotesPage"))
(defun my-visit-foo-sql () (interactive) (find-file "/tmp/foo.sql"))

(defun my-run-framework-testcase ()
  (interactive)
  (compile (concat "ant -e -s build.xml -Dtestclass="
		   (path-to-java-package (buffer-file-name)) "."
		   (file-name-sans-extension (file-name-nondirectory
					      (buffer-file-name)))
		   " test-one")))

(define-key global-map [f4] 'my-visit-work-todo)
(define-key global-map [\C-f4] 'my-visit-home-todo)
(define-key global-map [f6] 'my-visit-work-notes-page)
(define-key global-map [\C-f6] 'my-visit-home-notes-page)
(add-hook 'java-mode-hook
	  '(lambda ()
	     (define-key java-mode-map "\C-cr" 'my-run-framework-testcase)
	     (define-key java-mode-map "\C-c\C-r" 'my-run-framework-testcase)))

(setq sql-user "jimm")
(setq sql-server "localhost")
(setq sql-database "mediafactory_dev_jimm")

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(defvar compile-maven-hc-stage
  "cd ~/src/pasites/stage/harpercollins && maven -Eb dev-local")
(defvar compile-maven-hc-trunk
  "cd ~/src/pasites/trunk/harpercollins && maven -Eb dev-local")
(defvar compile-maven-bb-stage
  "cd ~/src/pasites/stage/blackbelt && maven -Eb dev-local")
(defvar compile-maven-bb-trunk
  "cd ~/src/pasites/trunk/blackbelt && maven -Eb dev-local")
(defvar compile-maven-mf-trunk
  "cd ~/src/mediafactory/trunk/mediafactory && maven -Eb install war-deploy")
(defvar compile-ant "ant -e -s build.xml ")
(defvar compile-ant-test "ant -e -s build.xml -Dbuild.env=test test")

(set-register ?l compile-maven-mf-trunk)
(set-register ?n compile-ant)
(set-register ?t compile-ant-test)
(setq compile-command (get-register ?n))

(custom-set-variables
 '(pmd-java-home "/usr/bin/java"))

(server-start)
