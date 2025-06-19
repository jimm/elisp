(load-theme 'jim-light)

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

(defun smoke-tests (&optional skip-tests)
  "Runs the quark smoke tests."
  (interactive "sTests to skip (management, monolith-{default,alternate,nucleus}): ")
  (let ((default-directory (getenv "quark")))
    (compile (concat "TESTS_TO_SKIP=" skip-tests " smoke"))))

(add-to-list 'auto-mode-alist '("\\.proto$" . javascript-mode)) ; good enough for now
