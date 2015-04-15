(defun zoom-frame-width-cols ()
 "I need to override this because when I'm hooked up to multiple monitors,
`display-pixel-width' returns the wrong value."
 (interactive)
  176)

;; (setq sql-user "jimm")
;; (setq sql-server "localhost")
;; (setq sql-database "db")

(setq dired-use-ls-dired nil)

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(setq sql-sqlite-program "sqlite3")

(when (fboundp #'deft)
  (setq deft-directory *my-work-orgs-dir*))

;; Markdown
(add-hook 'markdown-mode-hook
          (lambda () (setq markdown-command "multimarkdown")))

(defun remove-colorization ()
  "Remove colorization from the current buffer."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only nil))
      (replace-regexp "\\[[0-9]+m" "" nil (point-min) (point-max)))))

;; http://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Display ANSI color codes correctly in the *compilation* buffer."
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Run Rspec test in $candi.
(defun run-spec (fname)
  "Run an Rspec test from the $candi directory."
  (interactive "F")                     ; possibly nonexistent file name so we can append ":NNN"
  (compile (concat "cd $candi && RAILS_ENV=test bundle exec bin/rspec " fname)))

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
