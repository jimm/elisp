(setq eshell-history-size 512)

;;; Select a random sig as the eshell banner.
(let ((sigfile (concat *my-pim-dir* "signatures")))
  (when (file-exists-p sigfile)
    (let ((sigs (with-temp-buffer
                  (insert-file-contents sigfile)
                  (split-string (buffer-string) "\n\n" t))))
      (setq eshell-banner-message
            (concat (nth (random (length sigs)) sigs) "\n\n")))))

(require 'em-hist)			; So the history vars are defined
(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t)) ; Don't ask, just save
(if (boundp 'eshell-ask-to-save-history)
    (setq eshell-ask-to-save-history 'always)) ; For older(?) version

(defun eshell/ef (fname-regexp &optional dir)
  (ef fname-regexp (or dir default-directory)))


;;; ---- path manipulation

(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
	 (home-len (length home)))
    (if (and
	 (>= (length pwd) home-len)
	 (equal home (substring pwd 0 home-len)))
	(concat "~" (substring pwd home-len))
      pwd)))

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (concat "["
              (if (> (length git-output) 0)
                  (substring git-output 0 -1)
                "(no branch)")
              "] "))))

(defun curr-dir-svn-string (pwd)
  "Returns current subversion branch as a string, or the empty
string if PWD is not in a subversion repo (or the subversion
command is not found)."
  (interactive)
  (when (and (eshell-search-path "svn")
             (locate-dominating-file pwd ".svn"))
    (concat "[s:"
            (cond ((string-match-p "/trunk\\(/.*\\)?" pwd)
                   "trunk")
                  ((string-match "/branches/\\([^/]+\\)\\(/.*\\)?" pwd)
                   (match-string 1 pwd))
                  (t
                   "(no branch)"))
            "] ")))

(defun chop-path (path-list n)
  "Joins elements of PATH-LIST with \"/\". All but the last N
elements are abbreviated to their first letters."
  (flet ((shorten (elm) (if (zerop (length elm)) ""
                          (substring elm 0 1))))
    (if (> (length path-list) n)
        (concat
         (mapconcat #'shorten (butlast path-list n) "/")
         "/"
         (mapconcat #'identity (last path-list n) "/"))
      (mapconcat #'identity (last path-list 3) "/"))
    (mapconcat #'identity path-list "/")))

(setq eshell-prompt-regexp (if (boundp 'shell-prompt-pattern)
                               shell-prompt-pattern
                             "^[^#$%>\n]*[#$%>] *")
      eshell-prompt-function
      (lambda ()
        (concat
         (format-time-string "%H:%M:%S")
         " "
         (or (curr-dir-git-branch-string (eshell/pwd))
             (curr-dir-svn-string (eshell/pwd)))
         (chop-path (split-string (pwd-repl-home (eshell/pwd)) "/") 3)
         "\n"
         (if (= (user-uid) 0) "#" "$")
         " ")))

;; ; From http://www.emacswiki.org/cgi-bin/wiki.pl/EshellWThirtyTwo
;; ; Return nil, otherwise you'll see the return from w32-shell-execute
;; (defun eshell/open (file)
;;   "Invoke (w32-shell-execute \"Open\" FILE) and substitute slashes for
;; backslashes"
;;   (w32-shell-execute "Open" (substitute ?\\ ?/ (expand-file-name file)))
;;   nil)

(add-hook 'eshell-mode-hook
	  (lambda ()
            (local-set-key "\C-c\C-q" 'eshell-kill-process)
            (local-set-key "\C-c\C-k" 'compile)))
