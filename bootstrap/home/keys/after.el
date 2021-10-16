(set-my-theme 'light)

(defvar work-orgs-dir "warby_parker"
  "Name of $pim/orgs/work subdir where I keep work-related Org mode files.")

(setq dired-use-ls-dired nil)

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(setq sql-sqlite-program "sqlite3")


;; Org Mode repo: link
(defun display-repo-link (repo-name)
  (concat "https://github.com/jimm/" repo-name))

(add-to-list 'org-link-abbrev-alist
             '("repo" . "%(display-repo-link)"))

;; Start Emacs server
(unless (server-running-p)
  (server-start))
