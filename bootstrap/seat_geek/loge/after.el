(setq org-agenda-files (list
                        (concat *my-pim-dir* "orgs/work/seat_geek/todo.org")
                        (concat *my-pim-dir* "orgs/todo.org"))
      *my-eshell-vcs-maxlen* 20
      *my-eshell-vcs-del-prefix* "jm-"
      *status-file* (concat *my-pim-dir*
                            "orgs/work/seat_geek/status_"
                            (format-time-string "%Y")
                            ".org")
      emms-source-file-default-directory "~/Documents/Dropbox/Music/music/"
      Buffer-menu-name-width 24
      user-email-address "jmenard@seatgeek.com"
      rubocopfmt-use-bundler-when-possible nil
      ;; https://docspring.com/blog/posts/making-rubocop-20-times-faster/
      rubocopfmt-rubocop-command "rubocop-daemon-wrapper")
      ;; ns-right-command-modifier 'meta   ; for Win kbd at office

(load-theme 'jim-light)
(set-background-color "gray95")
(set-face-attribute 'org-block-begin-line nil :background "gray90")
(set-face-attribute 'org-block-end-line nil :background "gray90")

(use-package chatgpt-shell
  :defer t
  :config
  (setq chatgpt-shell-openai-key (getenv "OPENAI_API_KEY")
        chatgpt-shell-models (cons
                              (chatgpt-shell-openai-make-model
                               :version "gtp-4o"
                               :token-width 3
                               :context-window 12800)
                              chatgpt-shell-models)))


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

(put 'my-org-mode-repo-link 'org-link-abbrev-safe t)

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

(put 'my-org-mode-jira-link 'org-link-abbrev-safe t)

;; Org Mode mr: link

(defvar sg-mr-abbreviations-alist
  '(("lg" . "platform/letsgo")
    ("api" . "consumer/api")
    ("pp" . "consumer/peakpass")
    ("sg" . "consumer/seatgeek")
    ("box" . "jmenard/sandbox")
    ("rsc" . "consumer/rescraper")
    ("perf" . "consumer/auto-perf-test")
    ("sro" . "sro/sro4")))

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

(put 'my-org-mode-mr-link 'org-link-abbrev-safe t)

(defun wiki-home ()
"Opens the SeatGeek wiki home page."
  (interactive)
  (browse-url "https://seatgeek.atlassian.net/wiki/home"))

(defun wiki-search (search-text)
"Performs a search on the SeatGeek wiki using SEARCH-TEXT."
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

(defun -api-test-name ()
  (let* ((test-path (path-from-git-root-to-clipboard-kill-ring 1))
         (curr-func (which-function)))
    (if (and (> (or arg 1) 1) curr-func) ; arg given and we are in a func
        (concat test-path "::" (replace-regexp-in-string "\\." "::" curr-func))
      test-path)))

(defun api-checks (script-args)
  "Runs my api-checks script in the API code directory."
  (interactive "sapi-checks arguments: ")
  (let ((default-directory
          (locate-dominating-file (buffer-file-name (current-buffer)) ".git")))
    (compile (concat "api-checks " script-args))))

(defun api-tests (&optional arg)
  "Runs the test in the current buffer's file by sending the proper command to
a terminal.

With an ARG, append the line number at point.

If it has not already been called, `api-start' is run to create
the buffer and start the API poetry shell."
  (interactive "p")
  (let ((default-directory
          (locate-dominating-file (buffer-file-name (current-buffer)) ".git")))
    (compile (concat "api-tests " (-api-test-name)))))

(defun mr-description ()
  "Open a temp file containing the MR text template."
  (interactive)
  (find-file (make-temp-file "/tmp/mr-description-" nil ".md"))
  (insert-file (concat (getenv "HOME") "/Documents/merge_request_template.md"))
  (goto-char (point-min))
  (forward-line 2))

(defun store-link-and-path (&optional arg)
  "Calls `org-store-link` and saves the path from the repo root
to current file as a dotted module path into register p."
  (interactive)
  (org-store-link arg t)
  (let ((dir-path-to-file (-git-path-to-current-file)))
    (set-register ?p
                  (replace-regexp-in-string
                   "/"
                   "."
                   (file-name-sans-extension dir-path-to-file)))))

;;; ================ key bindings ================

(require 'status)
(global-set-key [f4] #'store-link-and-path)
(global-set-key [f5] #'status)
(global-set-key [f8] #'api-tests)
