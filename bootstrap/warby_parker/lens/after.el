(set-my-theme 'light)

(add-to-list 'auto-mode-alist '("\\.js\\.hbs$" . js-mode))

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
  '(("h" . "helios")
    ("ios" . "poe-ios")
    ("pmp" . "practice-management-platform")
    ("rom" . "retail-order-management")
    ("mc" . "manage-cloud")
    ("o" . "odin")
    ("p" . "puck")))

(defun wp-repo-link (repo-name)
  (concat "https://github.com/WarbyParker/" repo-name))

(defun wp-pr-link (tag)
  "Given a TAG of the form 'repo-number', returns a URL to a PR in that repo.

Repo names are either abbreviations or full repo names. Abbreviations must
be found in `wp-pr-abbreviations-alist'."
  (let* ((elems (split-string tag "-"))
         (repo (string-join (butlast elems) "-"))
         (pr-num (car (last elems)))
         (full-repo (alist-get repo wp-pr-abbreviations-alist repo nil #'equal)))
    (concat (wp-repo-link repo) "/pull/" pr-num)))

(add-to-list 'org-link-abbrev-alist
             '("jira" . "https://jira.warbyparker.com/browse/"))
(add-to-list 'org-link-abbrev-alist
             '("pr" . "%(wp-pr-link)"))
(add-to-list 'org-link-abbrev-alist
             '("repo" . "%(wp-repo-link)"))

;; Markdown
(add-hook 'markdown-mode-hook
          (lambda () (setq markdown-command "multimarkdown")))

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

(defun find-in-iso (regex)
  "Runs my `find-git' shell script against all the code in a known directory containing my team's repos."
  (interactive "ssearch regex: ")
  (let ((tempfile "/tmp/find-in-iso.grep"))
    (shell-command (concat "find-git -e -d " (getenv "iso") " " regex " > " tempfile))
    (find-file tempfile)))

;; Start Emacs server
(server-start)
