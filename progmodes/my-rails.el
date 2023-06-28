(require 'rails-find-other-file)

(when (fboundp #'projectile-rails-mode)
  (projectile-rails-global-mode)
  (add-hook 'projectile-rails-mode-hook
	    (lambda ()
	      (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map))))

(defun rails-root-p (path)
  "Returns true if path is a Rails root directory. Uses a heuristic that
involves looking for known Rails directories."
  (and path
       (file-directory-p path)
       (file-exists-p (concat path "Rakefile"))
       (file-directory-p (concat path "app"))
       (file-directory-p (concat path "config"))
       (file-directory-p (concat path "lib"))
       (file-directory-p (concat path "public"))))

(defun find-rails-root (path)
  "Returns Rails root dir at or above PATH. Returns nil if PATH is nil or no
Rails root dir is found. Uses `rails-root-p'."
  (and path (locate-dominating-file path #'rails-root-p)))

(defun rails-shell-command (command &optional buffer-name-part)
  "Run a Rails command in a terminal window."
  (interactive)
  (let ((buffer-name
         (generate-new-buffer-name (concat "*rails-" (or buffer-name-part command) "*"))))
    (shell buffer-name)
    (insert (concat "cd " (find-rails-root default-directory)))
    (comint-send-input)
    (insert (concat "rails " command))
    (comint-send-input)))

(defun rails-server ()
  "Run a Rails server in a terminal window. Finds the Rails root
directory for the current buffer and starts the server from that
directory."
  (interactive)
  (rails-shell-command "server"))

(defun rails-console ()
  "Run a Rails console in a terminal window. Finds the Rails root
directory for the current buffer and starts the server from that
directory."
  (interactive)
  (rails-shell-command "console"))

(defun rdb (&optional rails-root rails-env)
  "Start a MySQL buffer using `rdb' to determine connection.
Given optional RAILS-ROOT (default: search for root from
default-directory) and RAILS-ENV (\"development\" by default),
read the database settings from config/database.yml, set sql-*
variable values, and call `sql-mysql'. Uses my `rdb' script to
output the config values, because it knows how to read the YAML
file."
  (interactive "DRails root: \nsRails environment [development]: ")
  (let* ((rails-root (val-or-default rails-root (find-rails-root (buffer-file-name))))
         (rails-env (val-or-default rails-env "development"))
         (cmd (concat "rdb -s -e " rails-env " -r " rails-root))
         (vars (shell-command-to-string cmd))
         (lines (split-string vars "\n" t))
         (db-settings (mapcar (lambda (line)
                                ;; Turn "foo=\"bar\"" into ("foo" . "bar")
                                (let ((keyval (split-string line "=")))
                                  (cons (car keyval) (substring (cadr keyval) 1 -1))))
                              lines)))
    (message "%S" db-settings)
    (setq sql-server   (cdr (assoc "host" db-settings))
          sql-port     (string-to-number (cdr (assoc "port" db-settings)))
          sql-database (cdr (assoc "database" db-settings))
          sql-user     (cdr (assoc "username" db-settings))
          sql-password (cdr (assoc "password" db-settings)))
    (sql-mysql)
    (sql-set-sqli-buffer-generally)))

;;; ================ running tests ================

(defun -spring-run-rspec (f arg)
  "Run \"spring rspec FILE\" by sending it to function F.

If ARG is positive, run only the test that the cursor is in."
  (let ((path (buffer-file-name)))
    (if (> arg 1)
        (setq path (concat path ":" (int-to-string (line-number-at-pos)))))
    (funcall f (concat "cd " (find-rails-root path) " && spring rspec " path))))

(defun spring-rspec-terminal (arg)
  "Run \"spring rspec FILE\" in an external terminal.

If ARG is positive, run only the test that the cursor is in."
  (interactive "p")
  (-spring-run-rspec #'send-to-terminal arg))

(defun spring-rspec-compile (arg)
  "Run \"spring rspec FILE\" in a compilation buffer.

If ARG is positive, run only the test that the cursor is in."
  (interactive "p")
  (-spring-run-rspec #'compile arg))

;;; ================ done ================

(provide 'my-rails)
