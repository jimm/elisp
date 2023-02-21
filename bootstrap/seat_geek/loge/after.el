(setq *status-file*
      (concat *my-pim-dir* "orgs/work/seat_geek/status_" (format-time-string "%Y") ".org"))
(load-file (concat *my-emacs-lib-dir* "bootstrap/work-common.el"))

(set-my-theme 'light)

(defvar work-orgs-dir "seat_geek"
  "Name of $pim/orgs/work subdir where I keep work-related Org mode files.")

(setq dired-use-ls-dired nil
      browse-url-generic-program "open"
      Man-switches "-M /usr/share/man:/usr/local/share/man"
      sql-sqlite-program "sqlite3"
      rubocopfmt-rubocop-command "rubocop-daemon-wrapper")

;; Org Mode repo: link
(defun display-repo-link (repo-name)
  (concat "https://github.com/jimm/" repo-name))

(add-to-list 'org-link-abbrev-alist
             '("repo" . "%(display-repo-link)"))

;; Start Emacs server
(unless (server-running-p)
  (server-start))
