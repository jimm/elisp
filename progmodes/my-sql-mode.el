(defun show-create-table (table-name)
  (interactive "sTable name: ")
  (goto-char (point-min))
  (search-forward-regexp (concat "create table `?" table-name))
  (recenter 0))

;;; currently unused
(defun my-sql-send-paragraph ()
  "If *my-sql-regex* and *my-sql-regex-replacement* are defined,
sends the current SQL paragraph with regex replaced by
replacement. If those variables are not defined, calls
sql-send-paragraph."
  (interactive)
  (if (and (boundp '*my-sql-regex*) (boundp '*my-sql-regex-replacement*))
      (let ((start (save-excursion (backward-paragraph) (point)))
	    (end (save-excursion (forward-paragraph) (point))))
	(sql-send-string
	 (replace-regexp-in-string
	  *my-sql-regex*
	  *my-sql-regex-replacement*
	  (buffer-substring-no-properties start end))))
    (sql-send-paragraph)))
  
(add-hook 'sql-mode-hook
          (lambda ()
            (define-key sql-mode-map "\C-ct" 'show-create-table)))
