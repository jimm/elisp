(setq eshell-history-size 512)
(setq eshell-prompt-regexp "^.*> ")

(load "em-hist")			; So the history vars are defined
(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t)) ; Don't ask, just save
;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)
(if (boundp 'eshell-ask-to-save-history)
    (setq eshell-ask-to-save-history 'always)) ; For older(?) version
;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)

(defun eshell/ef (fname-regexp &rest dir) (ef fname-regexp default-directory))


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
      (concat "[g:"
              (if (> (length git-output) 0)
                  (substring git-output 0 -1)
                "(no branch)")
              "] "))))

(defun curr-dir-svn-string (pwd)
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

(setq eshell-prompt-function
      (lambda ()
        (concat
         (or (curr-dir-git-branch-string (eshell/pwd))
             (curr-dir-svn-string (eshell/pwd)))
         ((lambda (p-lst)
            (if (> (length p-lst) 3)
                (concat
                 (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                            (substring elm 0 1)))
                            (butlast p-lst 3)
                            "/")
                 "/"
                 (mapconcat (lambda (elm) elm)
                            (last p-lst 3)
                            "/"))
              (mapconcat (lambda (elm) elm)
                         p-lst
                         "/")))
          (split-string (pwd-repl-home (eshell/pwd)) "/"))
         "> ")))

;; ; From http://www.emacswiki.org/cgi-bin/wiki.pl/EshellWThirtyTwo
;; ; Return nil, otherwise you'll see the return from w32-shell-execute
;; (defun eshell/open (file)
;;   "Invoke (w32-shell-execute \"Open\" FILE) and substitute slashes for
;; backslashes"
;;   (w32-shell-execute "Open" (substitute ?\\ ?/ (expand-file-name file)))
;;   nil)

(add-hook 'eshell-mode-hook
	  '(lambda ()
	     (local-set-key "\C-c\C-q" 'eshell-kill-process)
	     (local-set-key "\C-c\C-k" 'compile)))
