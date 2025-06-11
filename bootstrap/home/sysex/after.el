(load-theme 'jim-light)

(setq user-email-address "jim@jimmenard.com"
      dired-use-ls-dired nil
      browse-url-generic-program "open"
      Man-switches "-M /usr/share/man:/opt/homebrew/share/man"
      sql-sqlite-program "sqlite3"
      rubocopfmt-rubocop-command "rubocop-daemon-wrapper")

;; Org Mode repo: link
(defun my-org-mode-repo-link (repo-name)
  (concat "https://github.com/jimm/" repo-name))

(add-to-list 'org-link-abbrev-alist
             '("repo" . "%(my-org-mode-repo-link)"))

(put 'my-org-mode-repo-link 'org-link-abbrev-safe t)

;; Copilot
(use-package copilot-chat)

;;; ================ Particle Health consulting ================

(setq org-agenda-files (list
                        (concat *my-pim-dir* "orgs/work/seat_geek/todo.org")
                        (concat *my-pim-dir* "orgs/todo.org"))
      *status-file* (concat *my-pim-dir*
                            "orgs/work/particle_health/status_"
                            (format-time-string "%Y")
                            ".org"))

(defvar work-orgs-dir "particle_health"
  "Name of $pim/orgs/work subdir where I keep work-related Org mode files.")

(defun my-org-mode-ph-repo-link (repo-name)
  (concat "https://github.com/ParticleHealth/" repo-name))

(add-to-list 'org-link-abbrev-alist
             '("ph-repo" . "%(my-org-mode-ph-repo-link)"))

(put 'my-org-mode-ph-repo-link 'org-link-abbrev-safe t)

;; Org Mode pr: link

(defvar ph-pr-abbreviations-alist
  '(("q" . "quark")
    ("p" . "proton")
    ("g" . "gluon")))

(defun my-org-mode-ph-pr-link (tag)
  "Given a TAG of the form '<repo>-<number>', returns a URL to a PR in that repo.

Repo names are either abbreviations or full repo names.
Abbreviations must be found in `ph-pr-abbreviations-alist'."
  (let* ((elems (split-string tag "-"))
         (repo (string-join (butlast elems) "-"))
         (pr-num (car (last elems)))
         (full-repo (alist-get repo ph-pr-abbreviations-alist repo nil #'equal)))
    (concat (my-org-mode-ph-repo-link full-repo) "/pull/" pr-num)))

(add-to-list 'org-link-abbrev-alist
             '("ph-pr" . "%(my-org-mode-ph-pr-link)"))

(put 'my-org-mode-ph-pr-link 'org-link-abbrev-safe t)

(defun my-org-mode-ph-shortcut-link (tag)
  "Given a TAG of the form '<number>', returns a URL to a Shortcut ticket."
  (concat "https://app.shortcut.com/particlehealth/story/" tag))

(add-to-list 'org-link-abbrev-alist
             '("ph-sc" . "%(my-org-mode-ph-shortcut-link)"))

(put 'my-org-mode-ph-shortcut-link 'org-link-abbrev-safe t)
