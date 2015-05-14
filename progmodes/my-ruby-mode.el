(require 'rails-find-other-file)
(require 'rdoc-mode)

(defun rdb (rails-root &optional rails-env)
  "Given RAILS-ROOT and optional RAILS-ENV (\"development\" by
default), read the database settings from config/database.yml,
set sql-* variable values, and call `sql-mysql'. Uses my `rdb'
script to output the config values, because it knows how to read
the YAML file."
  (interactive "DRails root: \nsRails environment [development]: ")
  (let* ((rails-env (or (and rails-env (> (length rails-env) 1) rails-env)
                        "development"))
         (cmd (concat " rdb -p -e " rails-env " -r " rails-root))
         (vars (shell-command-to-string cmd))
         (lines (split-string vars "\n" t))
         (db-settings (mapcar (lambda (line)
                                ;; Turn "foo=bar" into ("foo" . "bar")
                                (let ((keyval (split-string line "=")))
                                  (cons (car keyval) (cadr keyval))))
                              lines)))
    (setq sql-server   (cdr (assoc "host" db-settings))
          sql-port     (string-to-int (cdr (assoc "port" db-settings)))
          sql-database (cdr (assoc "database" db-settings))
          sql-user     (cdr (assoc "username" db-settings))
          sql-password (cdr (assoc "password" db-settings)))
    (sql-mysql)
    (sql-set-sqli-buffer-generally)))

(defun rails-root-p (path)
  "Returns true if path is a Rails root directory. Uses a heuristic that
involves looking for known Rails directories."
  (and path
       (file-directory-p path)
       (file-directory-p (concat path "app"))
       (file-directory-p (concat path "test"))
       (file-directory-p (concat path "config"))
       (file-directory-p (concat path "components"))
       (file-directory-p (concat path "log"))))

(defun parent-dir (path)
  "Returns the parent directory of path. Handles trailing slashes."
  (file-name-directory (cond ((equal "/" (substring path -1))
			      (substring path 0 -1))
			     (t path))))

(defun find-rails-root (path)
  "Returns Rails root dir at or above path. Returns nil if path is nil or no
Rails root dir is found."
    (message path)
    (cond ((not path) nil)
	  ((rails-root-p path) path)
	  ((equal "/" path) nil)
	  (t (find-rails-root (parent-dir path)))))

(defun insert-ruby-hash-arrow ()
  (interactive "*")
  (insert "=>")
  (backward-char 2)
  (just-one-space)
  (forward-char 2)
  (just-one-space))

(defun run-ruby-test (test-name)
  "Will run TEST-NAME from the current buffer's file, which is
presumed to be a test file. If TEST-NAME is empty or nil, runs
all tests in the file.

If *ruby-test-inject-command* is defined it is run after changing
to the root dir and before running the test. For example, you can
use this to delete the log/test.log file."
  (interactive "sTest name (empty for all tests in the file): ")
  (let ((root-dir (locate-dominating-file (file-name-directory (buffer-file-name)) "Rakefile")))
    (if root-dir
        (let ((root-relative-file (substring (buffer-file-name) (length (expand-file-name root-dir)))))
          (save-buffer)
          (compile (concat "cd " (shell-quote-argument (expand-file-name root-dir))
                           (when (boundp '*ruby-test-inject-command*)
                             (concat " && " *ruby-test-inject-command*))
                           " && ruby -I test " root-relative-file
                           (when (> (length test-name) 0)
                             (concat " -n " test-name)))))
      (error "Can not find RAILS_ROOT (Rakefile not found)"))))

(defun run-ruby-spec (spec-name-fragment)
  "Will run all specs whose full names include SPEC-NAME-FRAGMENT
from the current buffer's file, which is presumed to be an RSpec
test file. If SPEC-NAME-FRAGMENT is empty or nil, runs all tests
in the file.

If *ruby-test-inject-command* is defined it is run after changing
to the root dir and before running the test. For example, you can
use this to delete the log/test.log file."
  (interactive "sSpec name fragment (empty for all tests in the file): ")
  (let ((root-dir (locate-dominating-file (file-name-directory (buffer-file-name)) "Rakefile")))
    (if root-dir
        (let ((root-relative-file (substring (buffer-file-name) (length (expand-file-name root-dir)))))
          (save-buffer)
          (compile (concat "cd " (shell-quote-argument (expand-file-name root-dir))
                           (when (boundp '*ruby-test-inject-command*)
                             (concat " && " *ruby-test-inject-command*))
                           " && rspec " root-relative-file
                           (when (> (length spec-name-fragment) 0)
                             (concat " -e " (shell-quote-argument spec-name-fragment))))))
      (error "Can not find RAILS_ROOT (Rakefile not found)"))))

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map "\r" 'newline-and-indent)
            (define-key ruby-mode-map "\M-\C-h" 'backward-kill-word)
            (define-key ruby-mode-map "\C-cr" 'executable-interpret)
            (define-key ruby-mode-map "\C-cd" 'debug-comment)
            (define-key ruby-mode-map "\C-ch" 'insert-ruby-hash-arrow)
            (define-key ruby-mode-map "\C-ct" 'run-ruby-test)
            (define-key ruby-mode-map "\C-cs" 'run-ruby-spec)
            (define-key ruby-mode-map "{" 'self-insert-command)
            (define-key ruby-mode-map "}" 'self-insert-command)
            (setq ruby-indent-level 2)
            (font-lock-mode 1)))
