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
      emms-source-file-default-directory "~/Documents/Dropbox/Music/music/"
      Buffer-menu-name-width 24
      user-email-address "jim@displaysocial.com"
      rubocopfmt-use-bundler-when-possible nil)

;; Github
(defvar display-pr-abbreviations-alist
  '(("e" . "EvacuationComplete")))

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

(defun display-github-open-current-buffer ()
  (interactive)
  (let* ((git-root-dir (expand-file-name (locate-dominating-file (buffer-file-name) ".git")))
         (git-dir-name (file-name-nondirectory (directory-file-name git-root-dir)))
         (ev-repo-p (equal "display" git-dir-name)))
    (github-open-current-buffer "tsu-social"
                                (if ev-repo-p "rails5.2" "master")
                                (if ev-repo-p "EvacuationComplete" git-dir-name))))

(defvar display-docker-buffer-name "*Display Docker*")

;; Local Docker development
(defun dd-start ()
"Opens a shell and starts the app Docker container."
  (interactive)
  (if (get-buffer display-docker-buffer-name)
      (switch-to-buffer display-docker-buffer-name)
    (progn
      (shell)
      (rename-buffer "*Display Docker*")
      (insert (concat "cd " (getenv "wd")))
      (comint-send-input)
      (insert "docker compose exec app /bin/bash")
      (comint-send-input))))

(defun dd-run-tests (&optional arg)
"Runs the test in the current buffer's file by sending the proper command to
display-docker-buffer-name.

With an `ARG', append the line number at point.

If it has not already been called, `dd-start' is run to create the bufffer and
attach to the Docker app container."
  (interactive "p")
  (let ((curr-buffer (current-buffer)))
    (unless (get-buffer display-docker-buffer-name)
      (dd-start)
      (switch-to-buffer curr-buffer)))
  (let ((test-path (path-from-git-root-to-clipboard-kill-ring arg)))
    (switch-to-buffer-other-window display-docker-buffer-name)
    (goto-char (point-max))
    (insert (concat "spring rspec " test-path))
    (comint-send-input)))

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
ssh prod  (interactive)
  (-ssh-ec2 "prod" "*prod*"))

;; Start Emacs server
;; Note: for some reason, #'server-running-p is not yet defined, though
;; #'server-start is. I tried requiring 'server but that didn't help.
;(unless (server-running-p)
  (server-start);)
