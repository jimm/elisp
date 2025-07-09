; (load-theme 'jim-light)
(set-background-color "GhostWhite")

(setq user-email-address "jim@jimmenard.com"
      dired-use-ls-dired nil
      browse-url-generic-program "open"
      Man-switches "-M /usr/share/man:/opt/homebrew/share/man"
      sql-sqlite-program "sqlite3"
      rubocopfmt-rubocop-command "rubocop-daemon-wrapper")

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

(defun smoke-tests--val (s)
  "Returns a smoke test skip string. Given a known abbreviation, returns
the full name. Otherwise returns `s`."
  (cond ((or (equal s "d") (equal s "default")) "monolith-default")
        ((or (equal s "a") (equal s "alternate")) "monolith-alternate")
        ((or (equal s "n") (equal s "nucleus")) "monolith-nucleus")
        ((or (equal s "v") (equal s "v2queryflow")) "monolith-v2queryflow")
        ((equal s "m") "management")
        s))

(defun smoke-tests (&optional skip-tests)
  "Runs the quark smoke tests, optionally skipping specified test groups."
  (interactive "sTests to run ([m]anagement,[d]efault,[a]lternate,[n]ucleus,[v]2queryflow,[A]ll): ")
  (let ((default-directory (getenv "quark"))
        (tests-to-skip (list "management" "monolith-default" "monolith-alternate"
                             "monolith-nucleus" "monolith-v2queryflow")))
    (if (equal skip-tests "")
        (setq tests-to-skip '())
      (mapc (lambda (ch)
              (cond ((equal ch ?A) (setq tests-to-skip '()))
                    ((equal ch ?m) (setq tests-to-skip (remove "management" tests-to-skip)))
                    ((equal ch ?d) (setq tests-to-skip (remove "monolith-default" tests-to-skip)))
                    ((equal ch ?a) (setq tests-to-skip (remove "monolith-alternate" tests-to-skip)))
                    ((equal ch ?n) (setq tests-to-skip (remove "monolith-nucleus" tests-to-skip)))
                    ((equal ch ?v) (setq tests-to-skip (remove "monolith-v2queryflow" tests-to-skip)))))
            (string-to-list skip-tests)))
    (compile (concat "TESTS_TO_SKIP=" (string-join tests-to-skip ",") " smoke"))))

(add-to-list 'auto-mode-alist '("\\.proto$" . javascript-mode)) ; good enough for now

;; Org mode ph-sc links

(defun my-org-mode-ph-shortcut-link (tag)
  "Given a TAG of the form '<number>', returns a URL to a Shortcut ticket."
  (concat "https://app.shortcut.com/particlehealth/story/" tag))

(add-to-list 'org-link-abbrev-alist
             '("ph-sc" . "%(my-org-mode-ph-shortcut-link)"))

(put 'my-org-mode-ph-shortcut-link 'org-link-abbrev-safe t)

;; ---------------- Claude ----------------
;; https://github.com/stevemolitor/claude-code.el

;; for eat terminal backend (the default):
(use-package eat :ensure t)

;; ;; for vterm terminal backend:
;; (use-package vterm :ensure t)

;; install claude-code.el
(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map))
