;; (defun zoom-frame-width-cols ()
;;   (interactive)          ; for testing
;;   268)                   ; 268 for really full screen, 260 with dock exposed

;; (setq sql-user "jimm")
;; (setq sql-server "localhost")
;; (setq sql-database "db")

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(setq sql-sqlite-program "sqlite3")

(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)$" . markdown-mode))

;; Start Emacs server
(server-start)

(global-set-key [f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/work/aig/todo.org"))))
(global-set-key [\C-f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/todo.org"))))
(global-set-key [f6]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/work/aig/notes.org"))))
(global-set-key [\C-f6]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/notes.org"))))
