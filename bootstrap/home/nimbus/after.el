(my-face 'font-lock-string-face "ForestGreen")

(defun zoom-frame-width-cols ()
  (interactive)				; for testing
  268)                                  ; 268 for really full screen, 260 with dock exposed

;; (setq sql-user "jimm")
;; (setq sql-server "localhost")
;; (setq sql-database "db")

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(defvar compile-ant "ant -e -s build.xml ")
(defvar compile-rake "rake test ")

(set-register ?n compile-ant)
(set-register ?q "rake test ")
(setq compile-command (get-register ?q))

(custom-set-variables
 '(pmd-java-home "/usr/bin/java"))

; Development (local) only. Username/password won't work anywhere else.
; So it doesn't matter that you can see this.
(setq sql-user "jo")
(setq sql-password "happyx2joyx2")
(setq sql-database "icarly_cms_development")

(server-start)

(global-set-key [\C-f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/kaplan/todo.org"))))
(global-set-key [f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/todo.org"))))
