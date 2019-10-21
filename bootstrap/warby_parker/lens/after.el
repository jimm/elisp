(set-my-theme 'light)

(add-to-list 'auto-mode-alist '("\\.js\\.hbs$" . js-mode))

(defun zoom-frame-width-cols ()
  "I need to override this because when I'm hooked up to multiple monitors,
`display-pixel-width' returns the wrong value."
  (interactive)
  176)

(setq org-agenda-files (list
                        (concat *my-pim-dir* "orgs/work/warby_parker/todo.org")
                        (concat *my-pim-dir* "orgs/todo.org"))
      *status-file* (concat *my-pim-dir*
                            "orgs/work/warby_parker/status_"
                            (format-time-string "%Y")
                            ".org")
      *my-eshell-vcs-maxlen* 32
      emms-source-file-default-directory "~/Documents/Dropbox/Music/music/"
      Buffer-menu-name-width 24)

;; Jira
(defvar wp-pr-abbreviations-alist
  '(("mc" . "manage-cloud")
    ("h" . "helios")
    ("o" . "odin")
    ("p" . "puck")))

(defun wp-pr-link (tag)
  "Given a TAG of the form 'repo-number', returns a URL to a PR in that repo.

Repo names are either abbreviations or full repo names. Abbreviations must
be found in `wp-pr-abbreviations-alist'."
  (let* ((elems (split-string tag "-"))
         (repo (string-join (butlast elems) "-"))
         (pr-num (car (last elems)))
         (full-repo (alist-get repo wp-pr-abbreviations-alist repo nil #'equal)))
    (concat "https://github.com/WarbyParker/" full-repo "/pull/" pr-num)))

(add-to-list 'org-link-abbrev-alist
             '("jira" . "https://jira.warbyparker.com/browse/"))
(add-to-list 'org-link-abbrev-alist
             '("pr" . "%(wp-pr-link)"))

;; Markdown
(add-hook 'markdown-mode-hook
          (lambda () (setq markdown-command "multimarkdown")))

;;; projectile-project-root returns nil if no dir found, but the original
;;; definition of fzf does not handle that.
(require 'fzf)
(if (fboundp #'projectile-project-root)
    (defun fzf ()
      "Starts a fzf session."
      (interactive)
      (fzf/start (or (condition-case err
                         (projectile-project-root)
                       (error
                        default-directory))
                     default-directory))
      (fzf/start default-directory)))

(defun htest (arg file-name)
  "Runs `FILE-NAME' (by default, the current buffer's file) as a
single test using my \"htest\" script. That script assumes that
`FILE-NAME' is within the helios code base.

If `ARG' is greater than 1, add the `-s' flag to htest, telling
it to skip db initialization."
  (interactive "p\nftest file: ")
  (let ((helios-dir (concat (getenv "helios") "/")))
    (compile (concat "cd $helios && htest "
                     (if (> arg 1) "-s " "")
                     (substring file-name (length helios-dir))))))

(defun wp-github-open-current-buffer ()
  (interactive)
  (github-open-current-buffer "WarbyParker"))

;; Start Emacs server
(server-start)
