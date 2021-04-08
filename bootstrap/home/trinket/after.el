(set-my-theme 'light)

(setq dired-use-ls-dired nil)

(setq sql-sqlite-program "sqlite3")

;; Start Emacs server
(unless (fboundp #'server-running-p)
  (load "/Application/Emacs.app/Contents/Resources/lisp/server"))
(unless (server-running-p)
  (server-start))

