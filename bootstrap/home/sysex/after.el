(set-my-colors 'light)

(defvar work-orgs-dir "warby_parker"
  "Name of $pim/orgs/work subdir where I keep work-related Org mode files.")

(setq dired-use-ls-dired nil)

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(setq sql-sqlite-program "sqlite3")

;; Start Emacs server
(server-start)
