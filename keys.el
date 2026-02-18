;;; Define global key bindings then load the local machine's override file
;;; if it exists.

(defmacro when-fboundp-keymap-global-set (k f)
  `(when (fboundp (function ,f))
     (keymap-global-set ,k (function ,f))))

(defmacro set-org-file-key (key file)
  "Map a KEY globally to one of my Org mode FILEs."
  `(keymap-global-set ,key
     (lambda ()
       (interactive)
       (find-file (concat *my-pim-dir* "orgs/" ,file)))))

(keymap-global-set "M-z" #'zap-upto-char)
(keymap-global-set "M-`" #'my-ff-find-other-file)
(keymap-global-set "C-c 1" #'find-grep-dired)
(keymap-global-set "C-c 2" #'rgrep)
(keymap-global-set "C-c 3" #'grep)
(keymap-global-set "C-c 4" #'git-grep)
(keymap-global-set "C-c C-d" #'git-root-dired)
(keymap-global-set "C-h" #'backward-delete-char)
(keymap-global-set "C-c n" #'org-capture)
(keymap-global-set "C-c x" #'executable-interpret)
(when window-system
  (keymap-global-set "C-x u" (lambda ()
                            (interactive)
                            (message "Use Ctrl-/ instead."))))
(keymap-global-set "C-x ?" #'help-for-help)
(keymap-global-set "C-x g" #'magit-status)
(keymap-global-set "C-x C-k" #'compile)
; (keymap-global-set "C-x C-m" #'open-email-client)
(keymap-global-set "C-x C-z" #'shrink-window)
(keymap-global-set "M-<space>" #'just-one-space)

(when-fboundp-keymap-global-set "M-'" avy-goto-subword-1)
(when-fboundp-keymap-global-set "M-g g" avy-goto-line)
(when-fboundp-keymap-global-set "M-g M-g" avy-goto-line)
(when-fboundp-keymap-global-set "C-x o" ace-window)
(when-fboundp-keymap-global-set "M-o" ace-window)

(when-fboundp-keymap-global-set "M-g o" dumb-jump-go-other-window)
(when-fboundp-keymap-global-set "M-g j" dumb-jump-go)
(when-fboundp-keymap-global-set "M-g k" dumb-jump-back)
(when-fboundp-keymap-global-set "M-g q" dumb-jump-quick-look)

(keymap-global-set "<f1>" (lambda ()
                       "runs `my-shell'"
                       (interactive)
                       (funcall my-shell)))
(keymap-global-set "C-<f1>" (lambda ()
                          "runs `my-alternate-shell'"
                          (interactive)
                          (funcall my-alternate-shell)))
(keymap-global-set "<f2>" #'git-grep)
(keymap-global-set "C-<f2>" #'git-grep-callers-python-ruby)
(keymap-global-set "<f3>" #'split-window-right-and-focus)
(keymap-global-set "C-<f3>" #'center-of-attention)
(set-org-file-key "<f4>" "todo.org")
(keymap-global-set "<f5>"
                (lambda ()
                  "switch to buffer `*inferior-lisp'"
                  (interactive)
                  (switch-to-buffer "*inferior-lisp*")))
(keymap-global-set "C-<f5>"
                (lambda ()
                  "switch to buffer `*SQL*'"
                  (interactive)
                  (switch-to-buffer "*SQL*")))
(keymap-global-set "<f7>" #'line-to-other-window)
(keymap-global-set "C-<f7>" #'send-current-line-to-terminal-and-next-line)

(keymap-global-set "<f8>" (lambda ()
                       "Revert the current buffer."
                       (interactive)
                       (revert-buffer t t)))
(keymap-global-set "C-<f8>" #'revert-all-buffers)

;; File-finding key bindings.
(let ((find-file-func (cond ((fboundp #'projectile-find-file)
                              #'projectile-find-file)
                             ((fboundp #'find-file-in-project)
                              #'find-file-in-project)
                            ((fboundp #'fzf-git)
                             #'fzf-git)
                            ((fboundp #'find-file-in-repository)
                             #'find-file-in-repository)
                            (t #'ef))))
  (keymap-global-set "<f9>" find-file-func)
  (keymap-global-set "C-<f9>" #'ef))

;; (keymap-global-set "<f10>" #'toggle-frame-maximized)
;; (keymap-global-set "C-<f10>" (lambda () (interactive) (set-frame-width nil 80)))
(keymap-global-set "<f11>" #'other-window)
(when-fboundp-keymap-global-set "C-<f11>" ace-swap-window)

(keymap-global-set "C-c l" #'org-store-link)

(keymap-global-set "C-c w" #'toggle-current-window-dedication)

;;; Smex mode
(when (fboundp #'smex-initialize)
  (keymap-global-set "M-x" #'smex)
  (keymap-global-set "M-X" #'smex-major-mode-commands)
  (keymap-global-set "C-c C-c M-x" #'execute-extended-command))

;;; Tried writing a loop to do this, but the lambdas captured the index
;;; variable so the wrong number was being used.
(keymap-global-set "M-1" (lambda () (interactive) (nth-other-window 0)))
(keymap-global-set "M-2" (lambda () (interactive) (nth-other-window 1)))
(keymap-global-set "M-3" (lambda () (interactive) (nth-other-window 2)))
(keymap-global-set "M-4" (lambda () (interactive) (nth-other-window 3)))
(keymap-global-set "M-5" (lambda () (interactive) (nth-other-window 4)))
(keymap-global-set "M-6" (lambda () (interactive) (nth-other-window 5)))
(keymap-global-set "M-7" (lambda () (interactive) (nth-other-window 6)))
(keymap-global-set "M-8" (lambda () (interactive) (nth-other-window 7)))

(when (fboundp #'projectile-global-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; browse-at-remote
(keymap-global-set "C-c g g" #'browse-at-remote)
(keymap-global-set "C-c g k" #'browse-at-remote-kill)

;;; Load local machine's keys.el if it exists.
(load-bootstrap-file-if-exists "keys")
