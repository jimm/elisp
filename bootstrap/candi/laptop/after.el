(defun zoom-frame-width-cols ()
 "I need to override this because when I'm hooked up to multiple monitors,
`display-pixel-width' returns the wrong value."
 (interactive)
  176)

;; (setq sql-user "jimm")
;; (setq sql-server "localhost")
;; (setq sql-database "db")

(setq dired-use-ls-dired nil)
(when-fboundp-call line-number-mode 1)  ; display 'em
(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(setq sql-sqlite-program "sqlite3")

;; Add to the list of directories and files to ignore from rgrep, grep-find,
;; and friends.
(add-to-list 'grep-find-ignored-directories "bundle")
(add-to-list 'grep-find-ignored-files "*[-.]min.js")

(when (fboundp #'deft)
  (setq deft-directory *my-work-orgs-dir*))

;; Markdown
(add-hook 'markdown-mode-hook
          (lambda () (setq markdown-command "multimarkdown")))

(defun remove-colorization ()
  "Remove ANSI color codes from the current buffer."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (replace-regexp "\\[[0-9]+m" "" nil (point-min) (point-max)))))

;; http://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
(require 'ansi-color)
(defun colorize-current-buffer ()
  "Display ANSI color codes correctly in the *compilation* buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-current-buffer)

;; Run Rspec test in $candi.
(defun run-spec (seed fname)
  "Run Rspec test FNAME from the $candi directory. If SEED is 1, $RANDOM will be used.
FNAME may contain extra line number info (e.g., 'foo.rb::42')."
  (interactive "p\nF") ; possibly nonexistent file name so we can append ":NNN"
  (let ((seed-str (if (equal seed 1) "$RANDOM" seed)))
    (compile (concat "cd $candi && echo > log/test.log && RAILS_ENV=test bundle exec bin/rspec --seed=" seed-str " " fname))))

(defvar ctest-cmd-prefix
  (concat "cd $candi && RAILS_ENV=test bundle exec "))
(defun ctest-db ()
  (interactive)
  (compile (concat "cd $candi && RAILS_ENV=test bundle exec bin/rake db:drop db:setup")))
(defun ctest-models (seed)
  (interactive "p")
  (run-spec seed "spec/models"))
(defun ctest-javascript ()
  (interactive)
  (compile (concat "cd $candi && RAILS_ENV=test bundle exec bin/rake spec:javascript")))
(defun ctest-f1 (seed)
  (interactive "p")
  (run-spec seed "spec/features -t feature_thread:1"))
(defun ctest-f2 (seed)
  (interactive "p")
  (run-spec seed "spec/features -t ~feature_thread:1"))

;; Start Emacs server
(server-start)

(global-set-key [f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-work-orgs-dir* "todo.org"))))
(global-set-key [\C-f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/todo.org"))))
(global-set-key [f5]
                (lambda () (interactive) (switch-to-buffer "*SQL*")))
(global-set-key [\C-f5]
                (lambda () (interactive) (switch-to-buffer "*inferior-lisp*")))
(global-set-key [f6]
  (lambda ()
    (interactive)
    (find-file (concat *my-work-orgs-dir* "notes.org"))))
(global-set-key [\C-f6]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/notes.org"))))

(when-fboundp-global-set-key [\C-f3] magit-status) ; I use f3 for Mouse Locator
(when-fboundp-global-set-key "\C-xo" switch-window)
(when-fboundp-global-set-key [f11]   switch-window)
