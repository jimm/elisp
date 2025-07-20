;;; A simple sql mode for GCP Spanner.
;;;
;;; When entering the connection parameters, "server" must be the GCP
;;; project id and "database" must be the instance and database names
;;; separated by a colon.

(require 'sql)

(add-to-list 'sql-product-alist
             '(spanner
               :name "Spanner"
               :free-software nil
               :font-lock sql-mode-postgres-font-lock-keywords
               :sqli-program sql-spanner-program
               :sqli-options sql-spanner-options
               :sqli-login sql-spanner-login-params
               :sqli-comint-func sql-comint-spanner
               :list-all ("\\d+" . "\\dS+")
               :list-table ("\\d+ %s" . "\\dS+ %s")
               ;; :completion-object sql-postgres-completion-object
               :prompt-regexp "^[-[:alnum:]_]*[-=][#>] "
               :prompt-length 5
               :prompt-cont-regexp "^[-[:alnum:]_]*[-'(][#>] "
               :statement sql-postgres-statement-starters
               :input-filter sql-remove-tabs-filter
               :terminator ("\\(^\\s-*\\\\g\\|;\\)" . "\\g")))


(defcustom sql-spanner-program "spanner-cli"
  "Command to start spanner-cli.

Starts `sql-interactive-mode' after doing some setup."
  :type 'file)

(defcustom sql-spanner-options '()
  "List of additional options for `sql-spanner-program'."
  :type '(repeat string)
  :version "0.1")

;; (defcustom sql-postgres-login-params
;;   `((user :default ,(user-login-name))
;;     ;; (database :default ,(user-login-name)
;;     ;;           :completion ,(completion-table-dynamic
;;     ;;                         (lambda (_) (sql-postgres-list-databases)))
;;               :must-match confirm)
;;     server)
;;   "List of login parameters needed to connect to Postgres."
;;   :type 'sql-login-params
;;   :version "26.1")


(defcustom sql-spanner-login-params
  `(server                              ; project
    database)                           ; instance:database
  "List of login parameters needed to connect to Spanner."
  :type 'sql-login-params
  :group 'SQL
  :version "0.1")

(defun sql-spanner (&optional buffer)
  (interactive "P")
  (sql-product-interactive 'spanner buffer))

(defun sql-comint-spanner (product options &optional buf-name)
  "Create comint buffer and connect to Spanner."
  (let ((params
         (append
          (list "-p" sql-server)
          (list "-i" (car (split-string sql-database ":")))
          (list "-d" (cadr (split-string sql-database ":")))
          options)))
    (sql-comint product params buf-name)))
