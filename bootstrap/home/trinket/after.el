(set-my-theme 'light)

(setq dired-use-ls-dired nil)

(setq sql-sqlite-program "sqlite3")

;; Start Emacs server
(unless (server-running-p)
  (server-start))

