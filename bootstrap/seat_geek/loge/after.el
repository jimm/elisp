(setq org-agenda-files (list
                        (concat *my-pim-dir* "orgs/work/seat_geek/todo.org")
                        (concat *my-pim-dir* "orgs/todo.org"))
      *status-file* (concat *my-pim-dir*
                            "orgs/work/seat_geek/status_"
                            (format-time-string "%Y")
                            ".org")
      *my-eshell-vcs-maxlen* 20
      *my-eshell-vcs-del-prefix* "jm-"
      emms-source-file-default-directory "~/Documents/Dropbox/Music/music/"
      Buffer-menu-name-width 24
      user-email-address "jmenard@seatgeek.com"
      rubocopfmt-use-bundler-when-possible nil
      ;; https://docspring.com/blog/posts/making-rubocop-20-times-faster/
      rubocopfmt-rubocop-command "rubocop-daemon-wrapper"
      ns-right-command-modifier 'meta)  ; for Win kbd at office

(set-my-theme 'light)

(defvar work-orgs-dir "seat_geek"
  "Name of $pim/orgs/work subdir where I keep work-related Org mode files.")

(setq dired-use-ls-dired nil
      browse-url-generic-program "open"
      Man-switches "-M /usr/share/man:/usr/local/share/man"
      sql-sqlite-program "sqlite3"
      rubocopfmt-rubocop-command "rubocop-daemon-wrapper")

;; Org Mode repo: link
(defun my-org-mode-repo-link (repo-name)
  (concat "https://gitlab.seatgeekadmin.com/" repo-name))

(add-to-list 'org-link-abbrev-alist
             '("repo" . "%(my-org-mode-repo-link)"))

;; Org Mode jira: link
(defvar sg-jira-abbreviations-alist
  '(("e" . "ENGMT")))

(defvar sg-jira-default-project "ENGMT"
  "Default Jira project name, used when there is no project name in
a jira: link.")

(defun my-org-mode-jira-link (tag)
  "Given a TAG of the form '<proj>-<number>', returns a URL to a
Jira ticket in that project. With no project, use
`display-jira-default-project` instead.

Project names are either local abbreviations or full Jira project
abbreviations. Abbreviations must be found in
`display-jira-abbreviations-alist'."
  (let ((elems (split-string tag "-")))
    (if (= 1 (length elems))
        (concat "https://seatgeek.atlassian.net/browse/"
                display-jira-default-project "-" tag)
      (let* ((proj (string-join (butlast elems) "-"))
             (ticket-num (car (last elems)))
             (full-proj (alist-get proj display-jira-abbreviations-alist proj nil #'equal)))
        (concat "https://tsu.atlassian.net/browse/" full-proj "-" ticket-num)))))

(add-to-list 'org-link-abbrev-alist
             '("jira" . "%(my-org-mode-jira-link)"))

;; Org Mode pr: link

(defvar sg-pr-abbreviations-alist
  '(("lg" . "platform/letsgo")
    ("box" . "jmenard/sandbox")))

(defun sg-repo-link (repo-name)
  (concat "https://gitlab.seatgeekadmin.com/" repo-name))

(defun my-org-mode-pr-link (tag)
  "Given a TAG of the form '<repo>-<number>', returns a URL to a PR in that repo.

Repo names are either abbreviations or full repo names.
Abbreviations must be found in `sg-pr-abbreviations-alist'."
  (let* ((elems (split-string tag "-"))
         (repo (string-join (butlast elems) "-"))
         (pr-num (car (last elems)))
         (full-repo (alist-get repo sg-pr-abbreviations-alist repo nil #'equal)))
    (concat (sg-repo-link full-repo) "/-/merge_requests/" pr-num)))

(add-to-list 'org-link-abbrev-alist
             '("pr" . "%(my-org-mode-pr-link)"))

;; Start Emacs server
(unless (server-running-p)
  (server-start))
