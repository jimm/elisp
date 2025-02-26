(load-theme 'jim-light)

(defvar work-orgs-dir nil
  "Name of $pim/orgs/work subdir where I keep work-related Org mode files.")

(setq dired-use-ls-dired nil
      browse-url-generic-program "open"
      Man-switches "-M /usr/share/man:/usr/local/share/man"
      sql-sqlite-program "sqlite3"
      rubocopfmt-rubocop-command "rubocop-daemon-wrapper")

;; Org Mode repo: link
(defun my-org-mode-repo-link (repo-name)
  (concat "https://github.com/jimm/" repo-name))

(add-to-list 'org-link-abbrev-alist
             '("repo" . "%(my-org-mode-repo-link)"))
