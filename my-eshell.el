(setq eshell-history-size 512
      eshell-last-dir-ring-size 256)

(defvar *my-eshell-vcs-maxlen* nil
  "If defined, VCS branch names will be truncated to this width.")

(defvar *my-eshell-vcs-del-prefix* ""
  "If defined, a string that will be deleted from the beginning of VCS branch names.")

;;; Select a random sig as the eshell banner.
(when (file-exists-p *my-signature-file*)
    (setq eshell-banner-message '(concat (random-signature) "\n\n")))

(require 'em-hist)			; So the history vars are defined
(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t)) ; Don't ask, just save
(if (boundp 'eshell-ask-to-save-history)
    (setq eshell-ask-to-save-history 'always)) ; For older(?) version

(defun eshell/ef (fname-regexp &optional dir)
  (ef fname-regexp (or dir default-directory)))


;;; ---- path manipulation

(defun tildify-pwd (pwd)
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
    (let* ((git-output (replace-regexp-in-string "^refs/heads/" ""
                                                 (shell-command-to-string "git symbolic-ref HEAD")))
           (raw-branch-name (if (> (length git-output) 0) (substring git-output 0 -1) ""))
           (branch (cond
                    ((or
                      (string-equal raw-branch-name "master")
                      (string-equal raw-branch-name "main")
                      (string-equal raw-branch-name "trunk"))
                     "_")
                    ((string-match "fatal: ref HEAD is not a symbol" raw-branch-name)
                     "?")
                    ((> (length raw-branch-name) 0)
                     (string-remove-prefix *my-eshell-vcs-del-prefix* raw-branch-name))
                    (t
                     "?")))
           (truncated-branch (if *my-eshell-vcs-maxlen*
                                 (truncate-string-to-width branch *my-eshell-vcs-maxlen* nil nil t)
                               branch)))
      (concat "[" truncated-branch "] "))))

(defun curr-dir-svn-string (pwd)
  "Returns current subversion branch as a string, or the empty
string if PWD is not in a subversion repo (or the subversion
command is not found)."
  (interactive)
  (let* ((branch
          (cond ((string-match-p "/trunk\\(/.*\\)?" pwd)
                 "trunk")
                ((string-match "/branches/\\([^/]+\\)\\(/.*\\)?" pwd)
                 (match-string 1 pwd))
                (t
                 "(no branch)")))
         (truncated-branch (if *my-eshell-vcs-maxlen*
                               (truncate-string-to-width branch *my-eshell-vcs-maxlen* nil nil t)
                             branch)))
    (when (and (eshell-search-path "svn")
               (locate-dominating-file pwd ".svn"))
      (concat "[s:" truncated-branch "] "))))

(defun chop-path (path-list n)
  "Joins elements of PATH-LIST with \"/\". All but the last N
elements are abbreviated to their first letters."
  (cl-flet ((shorten (elm) (if (zerop (length elm)) ""
                             (substring elm 0 1))))
    (if (> (length path-list) n)
        (concat
         (mapconcat #'shorten (butlast path-list n) "/")
         "/"
         (mapconcat #'identity (last path-list n) "/"))
      (mapconcat #'identity path-list "/"))))

(setq eshell-prompt-regexp (if (boundp 'shell-prompt-pattern)
                               shell-prompt-pattern
                             "^[^#$%>\n]*[#$%>] *")
      eshell-prompt-function
      (lambda ()
        (let* ((vcs-str (or (curr-dir-git-branch-string (eshell/pwd))
                            (curr-dir-svn-string (eshell/pwd)))))
          (concat
           vcs-str
           (chop-path (split-string (tildify-pwd (eshell/pwd)) "/") 3)
           (if (= (user-uid) 0) "#" "$")
           " "))))

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
