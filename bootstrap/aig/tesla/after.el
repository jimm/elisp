;; (defun zoom-frame-width-cols ()
;;   (interactive)          ; for testing
;;   268)                   ; 268 for really full screen, 260 with dock exposed

;; (setq sql-user "jimm")
;; (setq sql-server "localhost")
;; (setq sql-database "db")

(setq dired-use-ls-dired nil)

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(load "status")
(setq *status-file* (substitute-in-file-name "$poa/status.org"))

(setq sql-sqlite-program "sqlite3")

(add-to-list 'webjump-sites
             '("slack" . "https://aig-science-dev.slack.com/"))
(add-to-list 'webjump-sites
             '("todo" . "https://en.todoist.com/"))

(when (fboundp #'deft)
  (setq deft-directory *my-aig-orgs-dir*))

(defun standup-window-config ()
  "Sets up windows for our stand-up meeting. Saves new window
configuration to register 'w'."
  (interactive)
  (let ((ff (lambda (name)
              (find-file (concat *my-aig-orgs-dir* name ".org"))
              (end-of-buffer))))

    (zoom-frame-width-cols)
    (zoom-frame-height-lines)

    (delete-other-windows)
    (funcall ff "cam")

    (split-window-right)
    (split-window-below)
    (other-window 1)
    (funcall ff "flier")

    (other-window 1)
    (funcall ff "mydesk")

    (split-window-below)
    (other-window 1)
    (funcall ff "devops")

    (other-window 1)
    (window-configuration-to-register ?w)))

;; Markdown
(add-hook 'markdown-mode-hook
          (lambda () (setq markdown-command "multimarkdown")))

;; Start Emacs server
(server-start)

(global-set-key [f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-aig-orgs-dir* "todo.org"))))
(global-set-key [\C-f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/todo.org"))))
(global-set-key [f6]
  (lambda ()
    (interactive)
    (find-file (concat *my-aig-orgs-dir* "notes.org"))))
(global-set-key [\C-f6]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/notes.org"))))

(global-set-key [f5] #'status)

(when-fboundp-global-set-key "\C-xo" switch-window)
(when-fboundp-global-set-key [f11]   switch-window)
