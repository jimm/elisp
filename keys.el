;;; Define global key bindings then load the local machine's override file
;;; if it exists.

(defmacro when-fboundp-global-set-key (k f)
  `(when (fboundp (function ,f))
     (global-set-key ,k (function ,f))))

(defmacro set-org-file-key (key file)
  "Map a KEY globally to one of my Org mode FILEs."
  `(global-set-key ,key
     (lambda ()
       (interactive)
       (find-file (concat *my-pim-dir* "orgs/" ,file)))))

(global-set-key "\M-z" #'zap-upto-char)
(global-set-key "\M-`" #'my-ff-find-other-file)
(global-set-key "\C-c1" #'find-grep-dired)
(global-set-key "\C-c2" #'rgrep)
(global-set-key "\C-c3" #'grep)
(global-set-key "\C-c4" #'git-grep)
(global-set-key "\C-c\C-d" #'git-root-dired)
(global-set-key "\C-h" #'backward-delete-char)
(global-set-key "\C-cn" #'org-capture)
(global-set-key "\C-cx" #'executable-interpret)
(global-set-key "\C-x?" #'help-for-help)
(global-set-key "\C-xg" #'magit-status)
(global-set-key "\C-x\C-k" #'compile)
(global-set-key "\C-x\C-m" #'open-email-client)
(global-set-key "\C-c\C-k" #'compile)
(global-set-key "\C-c\C-r" #'recompile)
(global-set-key "\C-cr" #'recompile)
(global-set-key "\C-x\C-z" #'shrink-window)
(global-set-key "\M- " #'just-one-space)

(when-fboundp-global-set-key "\M-'" avy-goto-subword-1)
(when-fboundp-global-set-key "\M-gg" avy-goto-line)
(when-fboundp-global-set-key "\M-g\M-g" avy-goto-line)
(when-fboundp-global-set-key "\C-xo" ace-window)
(when-fboundp-global-set-key "\M-o" ace-window)

(when-fboundp-global-set-key "\M-go" dumb-jump-go-other-window)
(when-fboundp-global-set-key "\M-gj" dumb-jump-go)
(when-fboundp-global-set-key "\M-gk" dumb-jump-back)
(when-fboundp-global-set-key "\M-gq" dumb-jump-quick-look)

(global-set-key [f1] my-shell)
(global-set-key [\C-f1] my-alternate-shell)
(global-set-key [f2] #'git-grep)
(global-set-key [\C-f2] #'git-grep-callers-python)
(set-org-file-key [f4] "todo.org")
(global-set-key [f5]
                (lambda () (interactive) (switch-to-buffer "*inferior-lisp*")))
(global-set-key [\C-f5]
                (lambda () (interactive) (switch-to-buffer "*SQL*")))
(global-set-key [f7] #'line-to-other-window)
(global-set-key [\C-f7] #'send-current-line-to-terminal-and-next-line)
(if (fboundp #'fzf-git)
    (progn
      (global-set-key [f9] #'fzf-git)
      (global-set-key [\C-f9] #'ef))
  (global-set-key [f9] #'ef))
(global-set-key [f10] #'zoom-frame)
(global-set-key [\C-f10] #'max-frame-height)
(global-set-key [f11] #'other-window)

(global-set-key "\C-cl" #'org-store-link)

(global-set-key "\C-cw" #'toggle-current-window-dedication)

;;; Smex mode
(when (fboundp #'smex-initialize)
  (global-set-key (kbd "M-x") #'smex)
  (global-set-key (kbd "M-X") #'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") #'execute-extended-command))

;;; Tried writing a loop to do this, but the final number wasn't being captured
(global-set-key "\M-1" (lambda () (interactive) (nth-other-window 0)))
(global-set-key "\M-2" (lambda () (interactive) (nth-other-window 1)))
(global-set-key "\M-3" (lambda () (interactive) (nth-other-window 2)))
(global-set-key "\M-4" (lambda () (interactive) (nth-other-window 3)))
(global-set-key "\M-5" (lambda () (interactive) (nth-other-window 4)))
(global-set-key "\M-6" (lambda () (interactive) (nth-other-window 5)))
(global-set-key "\M-7" (lambda () (interactive) (nth-other-window 6)))
(global-set-key "\M-8" (lambda () (interactive) (nth-other-window 7)))

(when (fboundp #'projectile-global-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;;; Load local machine's keys.el if it exists.

(load-init-if-exists "keys")
