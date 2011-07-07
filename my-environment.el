;;; Environment vars.

(defun my-read-env (all-env-vars)
  "Starts a subshess."
  (mapc (lambda (line)
          (when (string-match "\\([^=]+\\)=\\(.*\\)" line)
            (setenv (match-string 1 line) (match-string 2 line))))
        (split-string all-env-vars "\n")))
