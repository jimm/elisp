(defun my-emacs-wiki-home-icon ()
  "<a href='WelcomePage.html'><img src='../images/logo.gif'></a>")
(setq emacs-wiki-private-pages (cons "SharedSecrets" emacs-wiki-private-pages))

(defun my-visit-home-todo () (interactive)
  (find-file (concat *my-pim-dir* "wiki/ToDo")))
(defun my-visit-notes-page () (interactive) (emacs-wiki-find-file "NotesPage"))

(define-key global-map [\C-f4] 'my-visit-home-todo)
(define-key global-map [f6] 'my-visit-notes-page)

(setq eshell-scroll-show-maximum-output nil)
(setq transient-mark-mode nil)
(modify-coding-system-alist 'file "\\Secure\\'" 'no-conversion)
(modify-coding-system-alist 'file "\\.enc\\'" 'no-conversion)

;;; (setq sql-mysql-options '("--disable-pager" "--unbuffered" "--quick" "--table"
;;; 			  "--force" "--prompt='mysql> '" "--pager"))
(setq sql-mysql-options '("--print-defaults"))

(setq sql-user "jimm")
(setq sql-server "localhost")
(setq sql-database "backoffice_dev_jimm")

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
(defvar compile-ant "ant -emacs -find build.xml ")
(defvar compile-ant-test "ant -emacs -find build.xml -Dbuild.env=test test")

(set-register ?l compile-maven-mf-trunk)
(set-register ?n compile-ant)
(set-register ?t compile-ant-test)
(setq compile-command (get-register ?n))

(server-start)
