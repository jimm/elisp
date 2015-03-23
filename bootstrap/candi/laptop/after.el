;; (defun zoom-frame-width-cols ()
;;   (interactive)          ; for testing
;;   268)                   ; 268 for really full screen, 260 with dock exposed

;; (setq sql-user "jimm")
;; (setq sql-server "localhost")
;; (setq sql-database "db")

(setq dired-use-ls-dired nil)

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(load "status")
(setq *status-file* (substitute-in-file-name "$poa/status.org"))

(setq sql-sqlite-program "sqlite3")

(when (fboundp #'deft)
  (setq deft-directory *my-work-orgs-dir*))

;; Markdown
(add-hook 'markdown-mode-hook
          (lambda () (setq markdown-command "multimarkdown")))

;; Start Emacs server
(server-start)

(global-set-key [f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-work-orgs-dir* "todo.org"))))
(global-set-key [\C-f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/todo.org"))))
(global-set-key [f6]
  (lambda ()
    (interactive)
    (find-file (concat *my-work-orgs-dir* "notes.org"))))
(global-set-key [\C-f6]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/notes.org"))))

(global-set-key [f5] #'status)

(when-fboundp-global-set-key "\C-xo" switch-window)
(when-fboundp-global-set-key [f11]   switch-window)
