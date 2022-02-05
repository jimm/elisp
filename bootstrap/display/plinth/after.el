 (set-my-theme 'light)

(add-to-list 'auto-mode-alist '("\\.js\\.hbs$" . js-mode))

(setq org-agenda-files (list
                        (concat *my-pim-dir* "orgs/work/display/todo.org")
                        (concat *my-pim-dir* "orgs/todo.org"))
      *status-file* (concat *my-pim-dir*
                            "orgs/work/display/status_"
                            (format-time-string "%Y")
                            ".org")
      *my-eshell-vcs-maxlen* 32
      *my-eshell-vcs-del-prefix* "jimm/"
      emms-source-file-default-directory "~/Documents/Dropbox/Music/music/"
      Buffer-menu-name-width 24
      user-email-address "jim@displaysocial.com"
      rubocopfmt-use-bundler-when-possible nil
      ;; https://docspring.com/blog/posts/making-rubocop-20-times-faster/
      rubocopfmt-rubocop-command "rubocop-daemon-wrapper"
      ns-right-command-modifier 'meta)  ; for Win kbd at office

;; Github
(defvar display-pr-abbreviations-alist
  '(("e" . "EvacuationComplete")
    ("p" . "protosockets")
    ("ml" . "display-ml")
    ("mc" . "mediaconvert")))

(defun display-repo-link (repo-name)
  (concat "https://github.com/tsu-social/" repo-name))

(defun display-pr-link (tag)
  "Given a TAG of the form 'repo-number', returns a URL to a PR in that repo.

Repo names are either abbreviations or full repo names. Abbreviations must
be found in `display-pr-abbreviations-alist'."
  (let* ((elems (split-string tag "-"))
         (repo (string-join (butlast elems) "-"))
         (pr-num (car (last elems)))
         (full-repo (alist-get repo display-pr-abbreviations-alist repo nil #'equal)))
    (concat (display-repo-link full-repo) "/pull/" pr-num)))

(add-to-list 'org-link-abbrev-alist
             '("jira" . "https://tsu.atlassian.net/browse/"))
(add-to-list 'org-link-abbrev-alist
             '("pr" . "%(display-pr-link)"))
(add-to-list 'org-link-abbrev-alist
             '("repo" . "%(display-repo-link)"))

;; Markdown
(add-hook 'markdown-mode-hook
          (lambda () (setq markdown-command "multimarkdown")))

(defvar display-docker-buffer-name "*Display Docker*")
(defvar protosockets-docker-buffer-name "*Protosockets Docker*")

;;; Local Docker development

(defun -docker-start (buffer-name dir-env-var container-name)
"Opens a shell and starts the app Docker container."
  (interactive)
  (if (get-buffer buffer-name)
      (switch-to-buffer buffer-name)
    (progn
      (shell)
      (rename-buffer buffer-name)
      (insert (concat "cd " (getenv dir-env-var)))
      (comint-send-input)
      (insert (concat "docker compose exec " container-name " /bin/bash"))
      (comint-send-input))))

(defun dd-start ()
"Opens a shell and starts the app Docker container."
  (interactive)
  (-docker-start display-docker-buffer-name "wd" "app"))

(defun dd-run-tests (&optional arg)
"Runs the test in the current buffer's file by sending the proper command to
display-docker-buffer-name.

With an `ARG', append the line number at point.

If it has not already been called, `dd-start' is run to create the bufffer and
attach to the Docker app container."
  (interactive "p")
  (let ((curr-buffer (current-buffer)))
    (unless (get-buffer "*Display Docker*")
      (dd-start)
      (switch-to-buffer curr-buffer)))
  (let ((test-path (path-from-git-root-to-clipboard-kill-ring arg)))
    (switch-to-buffer-other-window display-docker-buffer-name)
    (goto-char (point-max))
    (insert (concat "spring rspec " test-path))
    (comint-send-input)))

(defun proto-start ()
"Opens a shell and starts the protosockets Docker container."
  (interactive)
  (-docker-start protosockets-docker-buffer-name "protocm" "dserver"))

(defun proto-run-tests (&optional arg)
"Runs the test in the current buffer's file by sending the proper command to
protosockets-docker-buffer-name.

With an `ARG', pass the `--onlyChanged' flag to Jest.

If it has not already been called, `proto-start' is run to create the bufffer and
attach to the Docker app container."
  (interactive "p")
  (let ((curr-buffer (current-buffer)))
    (unless (get-buffer protosockets-docker-buffer-name)
      (dd-start)
      (switch-to-buffer curr-buffer)))
  (switch-to-buffer-other-window protosockets-docker-buffer-name)
  (goto-char (point-max))
  (insert (concat "TRIVIA_ENV=test yarn test --color=false --detectOpenHandles"
                  (if (> (or arg 1) 1) " --onlyChanged" "")))
  (comint-send-input))

(defun -ssh-ec2 (name buffer-name)
  (interactive)
  (if (get-buffer buffer-name)
      (switch-to-buffer buffer-name)
    (progn
      (shell)
      (rename-buffer buffer-name)
      (goto-char (point-max))
      (insert (concat "ssh " name))
      (comint-send-input)
      (insert "jimm")
      (comint-send-input))))

(defun ssh-stage ()
  (interactive)
  (-ssh-ec2 "stage" "*stage*"))

(defalias #'ssh-test #'ssh-stage)

(defun ssh-prod ()
  (interactive)
  (-ssh-ec2 "prod" "*prod*"))

(defun open-jira (ticket-num)
  "Opens a work JIRA ticket, prompting for the ticket number if needed.
Project defaults to \"TS\"."
  (interactive "sJIRA ticket: ")
  (let* ((full-ticket-num (if (string-match "-" ticket-num)
                              ticket-num
                            (concat "TS-" ticket-num)))
         (url (concat "https://tsu.atlassian.net/browse/" full-ticket-num)))
    (browse-url-generic url)))

;; Start Emacs server
;; Note: for some reason, #'server-running-p is not yet defined, though
;; #'server-start is. I tried requiring 'server but that didn't help.
;(unless (server-running-p)
  (server-start);)
