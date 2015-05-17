(require 'rails-find-other-file)

(defun rails-root-p (path)
  "Returns true if path is a Rails root directory. Uses a heuristic that
involves looking for known Rails directories."
  (and path
       (file-directory-p path)
       (file-directory-p (concat path "Rakefile"))
       (file-directory-p (concat path "app"))
       (file-directory-p (concat path "config"))
       (file-directory-p (concat path "lib"))
       (file-directory-p (concat path "public"))
       (or (file-directory-p (concat path "test"))
           (file-directory-p (concat path "spec")))))

(defun find-rails-root (path)
  "Returns Rails root dir at or above path. Returns nil if path is nil or no
Rails root dir is found. Uses `rails-root-p'."
  (locate-dominating-file rails-root-p))

(defun rdb (&optional rails-root rails-env)
  "Given optional RAILS-ROOT (default: search for root from
default-directory) and RAILS-ENV (\"development\" by default),
read the database settings from config/database.yml, set sql-*
variable values, and call `sql-mysql'. Uses my `rdb' script to
output the config values, because it knows how to read the YAML
file."
  (interactive "DRails root: \nsRails environment [development]: ")
  (let* ((rails-root (val-or-default rails-root (find-rails-root)))
         (rails-env (val-or-default rails-env "development"))
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

(provide 'my-rails)
