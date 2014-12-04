;; (defun zoom-frame-width-cols ()
;;   (interactive)          ; for testing
;;   268)                   ; 268 for really full screen, 260 with dock exposed

;; (setq sql-user "jimm")
;; (setq sql-server "localhost")
;; (setq sql-database "db")

(setq dired-use-ls-dired nil)

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(defvar compile-ant "ant -e -s build.xml ")
(defvar compile-rake "rake test ")

(set-register ?n compile-ant)
(set-register ?q "rake test ")

(setq sql-sqlite-program "sqlite3")

(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)$" . markdown-mode))

;;
;; Google Chrome edit server
;;
;(require 'edit-server)
;(edit-server-start)
; does not work; asks to kill process anyway
;(add-hook 'kill-emacs-hook 'edit-server-stop)
; does not work
; (add-to-list 'kill-emacs-query-functions 'edit-server-stop)

;; Start Emacs server
(server-start)

(global-set-key [\C-f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/work/aig/todo.org"))))
(global-set-key [f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/todo.org"))))
(global-set-key [\C-f6]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/work/aig/notes.org"))))
