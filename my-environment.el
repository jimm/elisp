;;; Environment vars.

(defun my-read-env-files-in-dir (dir-path)
  "Calls my-read-env on all files in the directory dir-path."
  (interactive "DEnv files dir: ")
  (let ((files (directory-files dir-path t)))
    (setq files (my-remove-directories files)) ; Remove '.', '..', and directories
    (mapc 'my-read-env files)))		; Call my-read-env on remaining files

(defun my-remove-directories (file-list)
  "Removes all directories from file-list."
  (if file-list
      (if (file-directory-p (car file-list))
	  (my-remove-directories (cdr file-list))
	(cons (car file-list) (my-remove-directories (cdr file-list))))
    nil))

(defun my-read-env (env-file-path)
  "Reads a file containing bash environment variable definitions and sets
the environment variables."
  (interactive "fEnv file: ")
  (let ((buffer (get-buffer-create "*environment-vars*"))
	env-var
	env-value
	beg
	end)
    (with-current-buffer buffer
      (erase-buffer)
      (insert-file-contents env-file-path)
      (goto-char (point-min))
      (while (re-search-forward "^ *export" nil t)
	(progn
	  (forward-word 1)
	  (backward-word 1)
	  (setq beg (point))
	  (search-forward "=" nil t)
	  (backward-char)
	  (setq end (point))
	  (setq env-var (buffer-substring-no-properties beg end))

	  (forward-char)		; skip '='
	  (setq beg (point))
	  (end-of-line)
	  (setq end (point))
	  (setq env-value (buffer-substring-no-properties beg end))
	  ; (setenv env-var env-value nil t))))
; DEBUG
;(message "%s" (concat "setting " env-var " to " env-value))
	  (setenv env-var env-value t))))
; DEBUG
))
;    (kill-buffer buffer)))
