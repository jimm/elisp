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

(load-theme 'jim-light)

;;; Modify -git-url-and-branch-from-config to translate the URL domain. I
;;; don't know how to have git tell me what the `insteadOf` value is for the
;;; URL, so we do it the brute force way here.
(defvar sg-git-url-to-xlate "git@gitlab.service.seatgeek.mgmt:")
(defvar sg-git-xlated-url "https://gitlab.seatgeekadmin.com/")
(defun sg-git-url-modify (url-and-branch)
  (message "%s %S" "sg-git-url-modify: url-and-branch = " url-and-branch) ; DEBUG
  (message "%s %S" "sg-git-url-to-xlate" sg-git-url-to-xlate) ; DEBUG
  (message "%s %S" "sg-git-xlated-url" sg-git-xlated-url) ; DEBUG
  (let ((url (car url-and-branch))
        (branch (cadr url-and-branch))
        (to-xlate-len (length sg-git-url-to-xlate)))
    ;; TODO look for gitlab... up to colon
    ;; because I'm seeing both git@ and https://
    (message "%s %S %S" "(length url), to-xlate-len" (length url) to-xlate-len) ;DEBUG
    (message "%s %S" "(substring url 0 to-xlate-len)" (substring url 0 to-xlate-len)) ;DEBUG
    (if (and (> (length url) to-xlate-len)
             (string-equal sg-git-url-to-xlate (substring url 0 to-xlate-len)))
        (progn                          ; DEBUG
          (message "match, %s %S" "returning" (list (concat sg-git-xlated-url (substring url to-xlate-len)) branch)) ; DEBUG
        (list (concat sg-git-xlated-url (substring url to-xlate-len))
              branch)
        )                               ;DEBUG
      (progn                            ;DEBUG
        (message "%s %S" "no match, returning" url-and-branch)
      url-and-branch
      )                                 ;DEBUG
)))
(advice-add #'-git-url-and-branch-from-config :filter-return #'sg-git-url-modify)
; (advice-remove #'-git-url-and-branch-from-config #'sg-git-url-modify)

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
  '(("e" . "ENGMT")
    ("fl" . "FANDOML")))

(defvar sg-jira-default-project "ENGMT"
  "Default Jira project name, used when there is no project name in
a jira: link.")

(defun my-org-mode-jira-link (tag)
  "Given a TAG of the form '<proj>-<number>', returns a URL to a
Jira ticket in that project. With no project, use
`sg-jira-default-project` instead.

Project names are either local abbreviations or full Jira project
abbreviations. Abbreviations must be found in
`sg-jira-abbreviations-alist'."
  (let ((elems (split-string tag "-")))
    (if (= 1 (length elems))
        (concat "https://seatgeek.atlassian.net/browse/"
                sg-jira-default-project "-" tag)
      (let* ((proj (string-join (butlast elems) "-"))
             (ticket-num (car (last elems)))
             (full-proj (alist-get proj sg-jira-abbreviations-alist proj nil #'equal)))
        (concat "https://seatgeek.atlassian.net/browse/" full-proj "-" ticket-num)))))

(add-to-list 'org-link-abbrev-alist
             '("jira" . "%(my-org-mode-jira-link)"))

;; Org Mode pr: link

(defvar sg-pr-abbreviations-alist
  '(("lg" . "platform/letsgo")
    ("pp" . "consumer/peakpass")
    ("sg" . "consumer/seatgeek")
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

(defun wiki-home ()
"Opens the SeatGeek wiki home page."
  (interactive)
  (browse-url "https://seatgeek.atlassian.net/wiki/home"))

(defun wiki-search (search-text)
"Performs a search on the SeatGeek wiki using `SEARCH-TEXT'."
  (interactive "sSearch text: ")
  (browse-url
   (concat "https://seatgeek.atlassian.net/wiki/search?text="
           (url-hexify-string search-text))))
