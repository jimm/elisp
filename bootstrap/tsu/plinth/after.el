(set-my-theme 'light)

(add-to-list 'auto-mode-alist '("\\.js\\.hbs$" . js-mode))

(setq org-agenda-files (list
                        (concat *my-pim-dir* "orgs/work/tsu/todo.org")
                        (concat *my-pim-dir* "orgs/todo.org"))
      *status-file* (concat *my-pim-dir*
                            "orgs/work/tsu/status_"
                            (format-time-string "%Y")
                            ".org")
      *my-eshell-vcs-maxlen* 32
      emms-source-file-default-directory "~/Documents/Dropbox/Music/music/"
      Buffer-menu-name-width 24
      user-email-address "jim@tsusocial.com"
      rubocopfmt-use-bundler-when-possible nil)

;; Jira
(defvar tsu-pr-abbreviations-alist
  '(("e" . "EvacuationComplete")))

(defun tsu-repo-link (repo-name)
  (concat "https://github.com/tsu-social/" repo-name))

(defun tsu-pr-link (tag)
  "Given a TAG of the form 'repo-number', returns a URL to a PR in that repo.

Repo names are either abbreviations or full repo names. Abbreviations must
be found in `tsu-pr-abbreviations-alist'."
  (let* ((elems (split-string tag "-"))
         (repo (string-join (butlast elems) "-"))
         (pr-num (car (last elems)))
         (full-repo (alist-get repo tsu-pr-abbreviations-alist repo nil #'equal)))
    (concat (tsu-repo-link repo) "/pull/" pr-num)))

(add-to-list 'org-link-abbrev-alist
             '("pr" . "%(tsu-pr-link)"))
(add-to-list 'org-link-abbrev-alist
             '("repo" . "%(tsu-repo-link)"))

;; Markdown
(add-hook 'markdown-mode-hook
          (lambda () (setq markdown-command "multimarkdown")))

(defun tsu-github-open-current-buffer ()
  (interactive)
  (github-open-current-buffer "tsu-social"))

;; Start Emacs server
(unless (server-running-p)
  (server-start))
