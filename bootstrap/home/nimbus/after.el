(set-my-theme 'light)

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(setq sql-sqlite-program "sqlite3")

;; Start Emacs server
(unless (server-running-p)
  (server-start))
