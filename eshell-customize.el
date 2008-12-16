;; Note: move some of this into *my-emacs-lib-dir*/eshell/profile ? See
;; the function eshell-script-initialize in em-script.el.

(setq eshell-history-size 512)
(setq eshell-prompt-regexp "^[^>]*> ")
(make-face 'eshell-prompt-face)
(set-face-foreground 'eshell-prompt-face "FireBrick")
(set-face-bold-p 'eshell-prompt-face nil)

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

(setq eshell-prompt-function
      (lambda ()
	(concat ((lambda (p-lst)
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

; From http://www.emacswiki.org/cgi-bin/wiki.pl/EshellNavigation
; See also eshell-mode-hook below, with binding to eshell-bol-maybe-my.

 ;;;###autoload
(defun bol-maybe-general-my (prompt &optional alt-bol-fcn)
  (interactive)
  (if (and (string-match (concat "^" (regexp-quote prompt)
                                 " *$")
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (point)))
           (not (bolp)))
      (beginning-of-line)
    (if alt-bol-fcn
        (funcall alt-bol-fcn)
      (beginning-of-line)
      (search-forward-regexp prompt))))


;;;###autoload
(defun eshell-bol-maybe-my ()
  (interactive)
  (bol-maybe-general-my (funcall eshell-prompt-function)))


;; ; From http://www.emacswiki.org/cgi-bin/wiki.pl/EshellWThirtyTwo
;; ; Return nil, otherwise you'll see the return from w32-shell-execute
;; (defun eshell/open (file)
;;   "Invoke (w32-shell-execute \"Open\" FILE) and substitute slashes for
;; backslashes"
;;   (w32-shell-execute "Open" (substitute ?\\ ?/ (expand-file-name file)))
;;   nil)

(add-hook 'eshell-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-a") 'eshell-bol-maybe-my)
	     (local-set-key "\C-c\C-q" 'eshell-kill-process)
	     (local-set-key "\C-c\C-k" 'compile)))
