(defun zoom-frame-width-cols ()
  (interactive)				; for testing
  (round (/ (float (display-pixel-width))
            (+ (float (frame-char-width)) 0.175))))

(defun embiggen ()
  (interactive)
  (set-face-attribute 'default nil :font "Monaco 18")
  (set-frame-width (zoom-frame-width-cols)))

(defun unembiggen ()
  (interactive)
  (set-face-attribute 'default nil :font "Monaco 13")
  (max-frame-height))

(setq sql-mysql-options (list "-A"))
;; (setq sql-user "jimm")
;; (setq sql-server "localhost")
;; (setq sql-database "db")

(require 'mmm-defaults)

(setq dired-use-ls-dired nil)
(add-hook 'tramp-mode-hook
          (lambda ()
            (add-to-list 'tramp-default-user-alist '("ssh\\|scp" "10\\.10\\.*" "vulcan"))))
(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(defvar compile-ant "ant -e -s build.xml ")
(defvar compile-rake "rake test ")

(set-register ?n compile-ant)
(set-register ?q "rake test ")

(setq sql-sqlite-program "sqlite3")

(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)$" . markdown-mode))
(setq markdown-command "multimarkdown")

;; Start Emacs server
(server-start)

(global-set-key [f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/work/nrelate/todo.org"))))
(global-set-key [\C-f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/todo.org"))))
(global-set-key [f6]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/work/nrelate/notes.org"))))
(global-set-key [\C-f6]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/notes.org"))))
