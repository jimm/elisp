(defun zoom-frame-width-cols ()
  (interactive)				; for testing
  268)                                  ; 268 for really full screen, 260 with dock exposed

(setq edit-server-new-frame-height 77)

;; Standard Java indent level
(add-hook 'java-mode-hook (lambda () (setq c-basic-offset 4)))

;; this is dev-only info; won't work in production of course
(setq sql-user "brain")                 ; Oracle dev instance
(setq sql-password "brain")
(setq sql-database "oraclexe")
(setq sql-server "localhost")           ; MySQL
(setq sql-sqlite-program "sqlite3")     ; SQLite

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(defvar compile-ant "ant -e -s build.xml ")
(defvar compile-rake "rake test ")
(defvar compile-brain "cd $brain && mvn clean install")

(set-register ?n compile-ant)
(set-register ?q compile-rake)
(set-register ?m compile-brain)

(setq compile-command compile-brain)

(defvar sa-db
  "/Users/jimm/src/spine_align/db/development.sqlite3")
(defvar sa-db-perforce
  "/Users/jimm/Documents/Perforce/brain/brain/spine_align/db/development.sqlite3")
(defvar st-db
  "/Users/jimm/src/sandbox/taxonomy/SpineTagger/spineTagger.db")
(defvar st-db-perforce
  "/Users/jimm/Documents/Perforce/prfny3a01_1666/Jim_Menard_neuron/depot/CoreTech/Brain/jimm_sandbox/taxonomy/SpineTagger/spineTagger.db")
(set-register ?d sa-db)
(set-register ?e st-db)

(custom-set-variables
 '(pmd-java-home "/usr/bin/java"))

(defun oraclexe (cmd)
  (interactive "sCommand [start]: ")
  (let ((c (if (> (length cmd) 0) cmd "start")))
    (shell-command (concat "oraclexe " c))))

(defun play-test ()
  (interactive)
  (let ((eshell-buffer-name "play-server"))
    (eshell)
    (oraclexe "start")
    (insert "cd $brain")
    (eshell-send-input)
    (insert "play test")
    (eshell-send-input)))

(server-start)

(global-set-key [f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/kaplan/todo.org"))))
(global-set-key [\C-f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/todo.org"))))
(global-set-key [f6]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/kaplan/notes.org"))
    (goto-char (point-max))))
(global-set-key [\C-f6]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/notes.org"))
    (goto-char (point-max))))
(global-set-key [f11] 'other-window)
