(defun zoom-frame-width-cols ()
  (interactive)				; for testing
  268)                                  ; 268 for really full screen, 260 with dock exposed

(setq edit-server-new-frame-height 77)

(setq *my-javadoc-url*
      "file:///Users/jimm/Documents/documentation/java-api/api/")

;; Standard Java indent level
(add-hook 'java-mode-hook (lambda () (setq c-basic-offset 4)))

;; this is dev-only info; won't work in production of course
(setq sql-database "ideeli_development")
(setq sql-user "rails")
(setq sql-password "dev")
(setq sql-server "localhost")           ; MySQL
(setq sql-sqlite-program "sqlite3")     ; SQLite

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(defvar compile-ant "ant -e -s build.xml ")
(defvar compile-rake "rake test ")
(defvar compile-default "makeup ")

(set-register ?n compile-ant)
(set-register ?q compile-rake)
(set-register ?m compile-default)

(setq compile-command compile-default)

(setq org-agenda-files (list (concat *my-pim-dir* "orgs/todo.org")
			     (concat *my-pim-dir* "orgs/ideeli/todo.org")))

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

(global-set-key [f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/ideeli/todo.org"))))
(global-set-key [\C-f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/todo.org"))))
(global-set-key [f6]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/ideeli/notes.org"))
    (goto-char (point-max))))
(global-set-key [\C-f6]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/notes.org"))
    (goto-char (point-max))))
(global-set-key [f11] 'other-window)

; For run-ruby-test
(setq *ruby-test-inject-command* "drop_all_test_data.sh . && rm -f log/test.log")

; Reads all tables used in logs/test.log and inserts them at point as a
; single fixtures statement.
(defun copy-fixtures ()
  (interactive)
  (shell-command "cd $(dirname $(findup Rakefile)) && test_tables.rb | pbcopy")
  (yank)
  (exchange-point-and-mark)
  (insert "# ")
  (jw-rb-fill-comment-region)
  (forward-line 1)
  (forward-word 1)
  (backward-word 1)
  (backward-char 1)
  (insert "  ")
  (jw-rb-fill-comment-region)
  (forward-line -1)
  (set-mark-command 0)
  (delete-horizontal-space)
  (replace-string "# " "" nil (mark) (point))
  (search-backward "fixtures"))

;; ; Open a console
(defun console (&optional dir)
  (interactive "DRAILS_ROOT: ")
  (inf-ruby)
  (rename-buffer "*console*" t)
  (insert (concat "Dir.chdir('" (expand-file-name dir) "'); load 'script/console'"))
  (comint-send-input))

; My own "jcard:" link type
(defun my-org-jcard-open (card)
  "Open JIRA card."
  (my-url-open (concat "https://jira.ideeli.com/browse/" card)))

(when-fboundp-call org-add-link-type "jcard" 'my-org-jcard-open)
