;;; Path and environment vars.

(defun my-read-path (path-file-path)
  "Reads path-file-path and sets the PATH environment variable
from its contents."
  (interactive "fPath file: ")
  (let ((buffer (get-buffer-create "*path*"))
	path-element
	beg
	end)
    (with-current-buffer buffer
      (erase-buffer)
      (insert-file-contents path-file-path)
      (goto-char (point-min))
      (while (not (eobp))
	(progn
	  (setq beg (point))
	  (end-of-line)
	  (setq end (point))
	  (setq path-element (buffer-substring-no-properties beg end))
	  (forward-char)
	  (my-add-to-path path-element))))
    (kill-buffer buffer)))

(defun my-add-to-path (path-element)
  (interactive "Path element: ")
  (progn
    ; (setenv "PATH" (concat (getenv "PATH") ":" path-element) nil t)
    (setenv "PATH" (concat (getenv "PATH") ":" path-element) t)
    (setq exec-path (append exec-path (list path-element)))))

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
