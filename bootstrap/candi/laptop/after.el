(defun zoom-frame-width-cols ()
 "I need to override this because when I'm hooked up to multiple monitors,
`display-pixel-width' returns the wrong value."
 (interactive)
  176)

(when-fboundp-call line-number-mode 1)  ; display 'em

(setq dired-use-ls-dired nil
      browse-url-generic-program "open"
      Man-switches "-M /usr/share/man:/usr/local/share/man"
      sql-sqlite-program "sqlite3"
      *my-sql-regex* "^--.*"
      *my-sql-regex-replacement* "")

;; Add to the list of directories and files to ignore from rgrep, grep-find,
;; and friends.
(add-to-list 'grep-find-ignored-directories "bundle")
(add-to-list 'grep-find-ignored-files "*[-.]min.js")

(when (fboundp #'deft)
  (setq deft-directory *my-work-orgs-dir*))

;; Markdown
(add-hook 'markdown-mode-hook
          (lambda () (setq markdown-command "multimarkdown")))

;;; ================ running RSpec tests using ctest ================

;; TODO: have ctest-cmd open the appropriate log file and turn on
;; auto-revert-tail-mode.

(defvar candi--ctest-cmd-prefix
  (concat "cd $candi && ctest"))
(defun ctest-cmd (cmd &optional fname)
  "Compile using \"ctest CMD\" and open the FNAME output file using
`auto-revert-tail-mode'."
  (interactive "s")
  (switch-to-buffer "*compilation*")
  (compile (concat candi--ctest-cmd-prefix " " cmd))
  (sleep-for 1)                         ; wait for file creation
  (when fname
    (find-file (concat (getenv "tc") "/test_" fname ".txt"))
    (revert-buffer t t t)
    (auto-revert-tail-mode)))
(defun ctest-create-output-dir ()
  (interactive)
  (ctest-cmd "create-output-dir"))
(defun ctest-db ()
  (interactive)
  (ctest-cmd "db"))
(defun ctest-models ()
  (interactive)
  (ctest-cmd "models" "models"))
(defun ctest-javascript ()
  (interactive)
  (ctest-cmd "javascript" "javascript"))
(defun ctest-no-models ()
  (interactive)
  (ctest-cmd "no-models" "no_models"))
(defalias #'ctest-other #'ctest-no-models)
(defun ctest-f1 ()
  (interactive)
  (ctest-cmd "f1" "features_1"))
(defun ctest-f2 ()
  (interactive)
  (ctest-cmd "f2" "features_2"))

;; ================================================================

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
