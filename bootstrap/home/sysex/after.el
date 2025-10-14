(load-theme 'jim-light)

(setq user-email-address "jim@jimmenard.com"
      dired-use-ls-dired nil
      browse-url-generic-program "open"
      Man-switches "-M /usr/share/man:/opt/homebrew/share/man"
      sql-sqlite-program "sqlite3"
      rubocopfmt-rubocop-command "rubocop-daemon-wrapper"
      my-shell #'shell
      my-alternate-shell #'eshell)

;;; ================ Particle Health consulting ================

(let ((*my-emacs-bootstrap-domain* "particle_health")
      (*my-emacs-bootstrap-machine* "sysex"))
  (load-bootstrap-file-if-exists "particle-health"))
