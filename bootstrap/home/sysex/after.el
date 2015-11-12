(defvar work-orgs-dir "candi"
  "Name of $pim/orgs/work subdir where I keep work-related Org mode files.")

(setq dired-use-ls-dired nil)

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(defvar compile-ant "ant -e -s build.xml ")
(defvar compile-rake "rake test ")

(set-register ?n compile-ant)
(set-register ?q "rake test ")

(setq sql-sqlite-program "sqlite3")

(when (fboundp #'fzf)
  (setq fzf/executable "~/.fzf/bin/fzf"))

;; Fix for Max OS X 10.11.1 El Capitan problem
(setq visible-bell nil)
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit exit-minibuffer
                        mwheel-scroll down up next-line previous-line
                        backward-char forward-char))
          (invert-face 'mode-line)
          (run-with-timer 0.1 nil 'invert-face 'mode-line))))

;; (setq sql-user "jimm")
;; (setq sql-server "localhost")
;; (setq sql-database "db")

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
    (find-file (concat *my-pim-dir* (concat "orgs/work/" work-orgs-dir "/todo.org")))))
(global-set-key [f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/todo.org"))))
(global-set-key [\C-f6]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* (concat "orgs/work/" work-orgs-dir "/notes.org")))))
