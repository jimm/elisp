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

(defvar sg-git-internal-domain "gitlab.service.seatgeek.mgmt")
(defvar sg-git-external-domain "gitlab.seatgeekadmin.com")

(defun sg-git-url-modify (url-and-branch)
"Advice for -git-url-and-branch-from-config that translates the
URL domain. I don't know how to have git tell me what the
`insteadOf' value is for the URL, so we do it the brute force way
here."
  (let ((url (car url-and-branch))
        (branch (cadr url-and-branch)))
    (list (replace-regexp-in-string sg-git-internal-domain sg-git-external-domain url)
          branch)))

(advice-add #'-git-url-and-branch-from-config :filter-return #'sg-git-url-modify)
;; ; (advice-remove #'-git-url-and-branch-from-config #'sg-git-url-modify)

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

(defvar sg-mr-abbreviations-alist
  '(("lg" . "platform/letsgo")
    ("api" . "consumer/api")
    ("pp" . "consumer/peakpass")
    ("sg" . "consumer/seatgeek")
    ("box" . "jmenard/sandbox")))

(defun sg-repo-link (repo-name)
  (concat "https://gitlab.seatgeekadmin.com/" repo-name))

(defun my-org-mode-mr-link (tag)
  "Given a TAG of the form '<repo>-<number>', returns a URL to a MR in that repo.

Repo names are either abbreviations or full repo names.
Abbreviations must be found in `sg-mr-abbreviations-alist'."
  (let* ((elems (split-string tag "-"))
         (repo (string-join (butlast elems) "-"))
         (mr-num (car (last elems)))
         (full-repo (alist-get repo sg-mr-abbreviations-alist repo nil #'equal)))
    (concat (sg-repo-link full-repo) "/-/merge_requests/" mr-num)))

(add-to-list 'org-link-abbrev-alist
             '("mr" . "%(my-org-mode-mr-link)"))

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

;;; Local API development

(defvar sg-api-poetry-shell-buffer-name "*API Poetry Shell*")

(defun -poetry-start (buffer-name dir-env-var)
"Opens a shell and starts the API poetry shell."
  (interactive)
  (if (get-buffer buffer-name)
      (switch-to-buffer buffer-name)
    (progn
      (shell)
      (rename-buffer buffer-name)
      (insert (concat "cd " (getenv dir-env-var)))
      (comint-send-input)
      (insert "poetry shell")
      (comint-send-input))))

(defun api-start ()
"Opens a shell and starts the API poetry shell."
  (interactive)
  (-poetry-start sg-api-poetry-shell-buffer-name "api"))

(defun -api-test-name ()
  (let* ((test-path (path-from-git-root-to-clipboard-kill-ring 1))
         (curr-func (which-function)))
    (if (and (> (or arg 1) 1) curr-func) ; arg given and we are in a func
        (concat test-path "::" (replace-regexp-in-string "\\." "::" curr-func))
      test-path)))

(defun api-run-tests (&optional arg)
"Runs the test in the current buffer's file by sending the proper command to
sg-api-poetry-shell-buffer-name.

With an `ARG', append the line number at point.

If it has not already been called, `api-start' is run to create
the buffer and start the API poetry shell."
  (interactive "p")
  (unless (get-buffer sg-api-poetry-shell-buffer-name)
    (let ((curr-buffer (current-buffer)))
      (api-start)
      (switch-to-buffer curr-buffer)))
    (switch-to-buffer-other-window sg-api-poetry-shell-buffer-name t)
    (goto-char (point-max))
    (insert (concat "TEST_NAME=" (-api-test-name) " PYTEST=pytest make test"))
    (comint-send-input))

(defun api-compile-tests (&optional arg)
  "Runs the test in the current buffer's file by sending the proper command to
a terminal.

With an `ARG', append the line number at point.

If it has not already been called, `api-start' is run to create
the buffer and start the API poetry shell."
  (interactive "p")
  (let ((default-directory
          (locate-dominating-file (buffer-file-name (current-buffer)) ".git")))
    (compile (concat "TEST_NAME=" (-api-test-name) " PYTEST=pytest poetry run make test"))))
