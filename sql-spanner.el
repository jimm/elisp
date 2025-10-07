;;; A simple sql mode for GCP Spanner.
;;;
;;; When entering the connection parameters, "server" must be the GCP
;;; project id and "database" must be the instance and database names
;;; separated by a colon.
;;;
;;; In addition to doing the usual, it supports variable substitution. Any
;;; line in the input matching "@name = value" is stored in the hash table
;;; `sql-spanner-context' and turned into a comment. Any line starting with
;;; "unset @name" or "unset name" will remove that variable from
;;; `sql-spanner-context' and turned into a comment. When "@name" is seen in
;;; any other text, the value is substituted. Names must consist of
;;; alphanumeric characters or underscore.
;;;
;;; To support multi-line variable definitions, before processing
;;; backslashes and newlines at the ends of lines are removed, joining lines
;;; together.
;;;
;;; Bugs:
;;;
;;; - Renaming the buffer doesn't work; any existing or new SQL buffers
;;;   don't recognize it as an active SQL buffer.

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
							 ;; list-all ignores parent table information
               :list-all "SELECT t.table_name FROM information_schema.tables as t WHERE t.table_catalog = '' AND t.table_schema = '' ORDER BY t.table_name;"
							 ;; a simple list of column info, not a complete CREATE TABLE statement
               :list-table "SELECT c.column_name, c.spanner_type, c.is_nullable, c.column_default FROM information_schema.columns as c WHERE c.table_name = 'Queries' ORDER BY c.ordinal_position"
               ;; :completion-object sql-postgres-completion-object
               :prompt-regexp "^spanner> "
               :prompt-length 9
               :prompt-cont-regexp "^[-[:alnum:]_]*[-'(][#>] "
               :statement sql-postgres-statement-starters
               :input-filter sql-spanner-input-filter
               ;; If sql-send-terminator is t then this value is used. The
               ;; cons says that if there is no \G or ; at end then use \G.
               :terminator ("\\(\\\\G\\|;\\)" . "\\G")))

(defcustom sql-spanner-program "spanner-cli"
  "Command to start spanner-cli.

Starts `sql-interactive-mode' after doing some setup."
  :type 'file)

(defcustom sql-spanner-options nil
  "List of additional options for `sql-spanner-program'."
  :type '(repeat string)
  :version "0.1")

(defcustom sql-spanner-login-params
  '(user                                ; project id
    server                              ; instance
    database)                           ; database
  "List of login parameters needed to connect to Spanner. NOTE: user must
be project id, server must be instance name, and database is database
name."
  :type 'sql-login-params
  :group 'SQL
  :version "0.1")

(defun sql-spanner (&optional buffer)
  "Run Spanner as an inferior process."
  (interactive "P")
  (sql-product-interactive 'spanner buffer))

(defun sql-comint-spanner (product options &optional buf-name)
  "Create comint buffer and connect to Spanner."
  (let ((params (append (list "-p" sql-user)
                        (list "-i" sql-server)
                        (list "-d" sql-database)
                        options)))
    (sql-comint product params buf-name)))

;;; ================ variable substitution ================

(defcustom sql-spanner-context (make-hash-table :test #'equal)
  "Contans a hash table of variable names and values that get plugged in to
SQL statements."
	:group 'SQL
	:local t
	:version "0.1")

(defun sql-spanner-context-contents ()
  "Writes the contents of the context to a buffer for viewing."
  (interactive)
  (with-temp-buffer-window
      "*SQL Spanner Variables*"
      nil
      nil
      (maphash (lambda (key value)
                 (princ (format "%s:\t%s\n" key value)))
               sql-spanner-context)))

(defun -sql-spanner-process-bindings (line)
  "If `line' is a binding or unbinding then process it and turn it into a
comment, else leave it alone. Returns the possibly modified line.

Bindings look like
@name = value

Unbindings look like
unset @name (or unset name)
"
  (cond ((string-match
          (rx line-start
              ?@ (group (+ (or alnum ?_))) ; @name
              (* space) ?= (* space)       ; =
              (group (+ anything))         ; value
              eol)
          line)
         (progn
           (puthash (match-string 1 line) (match-string 2 line) sql-spanner-context)
           (concat "-- " line)))
        ((string-match
          (rx line-start "unset" (* space) (? ?@) (group (+ (or alnum ?_))))
          line)
         (progn
           (remhash (match-string 1 line) sql-spanner-context)
           (concat "-- " line)))
        (t (if (string-search "@" line)
               (-sql-spanner-substitute-bindings line)
             line))))

(defun -sql-spanner-substitute-bindings (line)
  "Replace all instances of @name with value from sql-spanner-context. If value is not found, @name will be preserved as-is."
  (message "substitute bindings line = %s" line)
  (let ((modified-line "")
        (start 0))
    (while-let ((found-offset (string-match (rx ?@ (group (+ (or alnum ?_)))) line)) ; @name
                (name (match-string 1 line)))
      (setq line (concat (substring line 0 found-offset)
                         (gethash name sql-spanner-context)
                         (substring line (+ found-offset 1 (length name))))))
    line))

(defun -sql-spanner-comment-or-empty (str)
  "Returns non-nil if `str' is both not the empty string and not a SQL
comment line."
  (or (string-empty-p str)
      (string-match (rx (* space) "--") str)))

(defun sql-spanner-input-filter (str)
	"Munges `str` before sending it to the SQL buffer by processing bindings."
	(let ((join-backslashed (string-replace "\\\n" "" str)))
		(mapconcat #'-sql-spanner-process-bindings (split-string join-backslashed "\n") "\n")))
