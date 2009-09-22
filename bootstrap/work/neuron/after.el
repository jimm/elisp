(defun zoom-frame-width-cols ()
  (interactive)				; for testing
  268)                                  ; 268 for really full screen, 260 with dock exposed

;; (setq sql-user "jimm")
;; (setq sql-server "localhost")
;; (setq sql-database "db")
(setq sql-sqlite-program "sqlite3")

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(defvar compile-ant "ant -e -s build.xml ")
(defvar compile-rake "rake test ")

(set-register ?n compile-ant)
(set-register ?q compile-rake)
(setq compile-command (get-register ?q))

(defvar sa-db
  "/Users/jimm/src/sandbox/taxonomy/spine_align/db/development.sqlite3")
(defvar sa-db-perforce
  "/Users/jimm/Documents/Perforce/prfny3a01_1666/Jim_Menard_neuron/depot/CoreTech/Brain/SpineAlign/db/development.sqlite3")
(defvar st-db
  "/Users/jimm/src/sandbox/taxonomy/SpineTagger/spineTagger.db")
(defvar st-db-perforce
  "/Users/jimm/Documents/Perforce/prfny3a01_1666/Jim_Menard_neuron/depot/CoreTech/Brain/jimm_sandbox/taxonomy/SpineTagger/spineTagger.db")
(set-register ?d sa-db)
(set-register ?e st-db)

(custom-set-variables
 '(pmd-java-home "/usr/bin/java"))

(server-start)

(global-set-key [f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/kaplan_todo.org"))))
(global-set-key [\C-f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/todo.org"))))

; TODO use light/dark versions code
(set-face-attribute 'org-level-1 nil :height 140 :foreground "white" :background "black" :bold t)
