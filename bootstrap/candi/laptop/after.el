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
    (replace-regexp "\\[[0-9]+m" "" nil (point-min) (point-max))))

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

(when-fboundp-global-set-key "\C-xo" switch-window)
(when-fboundp-global-set-key [f11]   switch-window)
