(setq sql-user "ga_cms")
(setq sql-server "localhost")
;; (setq sql-database "")

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)$" . markdown-mode))
(setq markdown-command "maruku")
(set-face-attribute 'markdown-header-face-1 nil :height 1.3 :bold t :foreground "Black")
(set-face-attribute 'markdown-header-face-2 nil :foreground "DarkGreen")
(set-face-attribute 'markdown-header-face-3 nil :foreground "Purple" :bold nil)
(set-face-attribute 'markdown-header-face-4 nil :foreground "Firebrick" :bold nil)
(set-face-attribute 'markdown-header-face-5 nil :foreground "SeaGreen" :bold nil)

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man:/opt/local/share/man")

(setq *my-javadoc-url* "file://localhost/Users/jimm/Documents/documentation/jdk_7/api/")

(setq sql-sqlite-program "sqlite3")

;; Restore old mouse copy behavior
(setq select-active-regions nil)
(setq mouse-drag-copy-region t)
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard nil)
;; To fully mimic the previous behavior, should also bind
;; `mouse-yank-at-click' to mouse-2.

;; Start Emacs server
(server-start)

(global-set-key [f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/general_assembly/todo.org"))))
(global-set-key [\C-f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/todo.org"))))
(global-set-key [f6]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/general_assembly/notes.org"))))
(global-set-key [\C-f6]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/notes.org"))))
