(load-theme 'jim-light)

(setq user-email-address "jim@jimmenard.com"
      dired-use-ls-dired nil
      browse-url-generic-program "open"
      Man-switches "-M /usr/share/man:/opt/homebrew/share/man"
      sql-sqlite-program "sqlite3"
      rubocopfmt-rubocop-command "rubocop-daemon-wrapper")

;; ;; Copilot
;; ;; (using Claude with PH)
;; (use-package copilot-chat)

;;; ================ Particle Health consulting ================

(load-bootstrap-file-if-exists "particle-health")
