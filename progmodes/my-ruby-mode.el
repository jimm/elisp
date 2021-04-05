(require 'my-rails)
(require 'rdoc-mode)
(require 'ruby-testing)
(require 'ob-ruby)                      ; Org-Babel support for Ruby eval

(defun insert-ruby-hash-arrow ()
  (interactive "*")
  (insert "=>")
  (backward-char 2)
  (just-one-space)
  (forward-char 2)
  (just-one-space))

(defun find-ruby-project-root (path)
  "Returns Ruby project root dir at or above PATH. Returns nil if
PATH is nil or no project root dir is found.

Looks for a Rails root directory using `rails-root-p` or a
Rakefile, .git directory, or .svn directory."
  (locate-dominating-file
   path
   (lambda (path)
     (or (rails-root-p path)
         (file-exists-p (concat path "Rakefile"))
         (file-exists-p (concat path "Gemfile"))
         (file-exists-p (concat path "README.rdoc"))
         (file-exists-p (concat path ".ruby-version"))
         (file-directory-p (concat path ".git/"))
         (file-directory-p (concat path ".svn/"))))))

(defun my-ruby-find-definition (word &optional start-dir)
  "Searches for \"def word_under_point\" from the root dir of the
current project."
  (interactive "sFind definition of: \nDStarting directory: ")
  (let ((root-dir (or start-dir (find-ruby-project-root (buffer-file-name)))))
    (when root-dir
      (rgrep (concat "def (self\.)?" word) "*.rb" root-dir))))

(defun my-ruby-find-definition-at-point ()
  (interactive)
  (my-ruby-find-definition (thing-at-point 'symbol)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map "\r" #'newline-and-indent)
            (define-key ruby-mode-map "\M-\C-h" #'backward-kill-word)
            (define-key ruby-mode-map "\C-cx" #'executable-interpret)
            (define-key ruby-mode-map "\C-cd" #'debug-comment)
            (define-key ruby-mode-map "\C-ch" #'insert-ruby-hash-arrow)
            (define-key ruby-mode-map "\C-c." #'my-ruby-find-definition-at-point)
            (define-key ruby-mode-map "{" #'self-insert-command)
            (define-key ruby-mode-map "}" #'self-insert-command)
            (setq ruby-insert-encoding-magic-comment nil)
            (setq ruby-indent-level 2)
            (font-lock-mode 1)))

(when (fboundp #'rubocopyfmt-mode)
  (add-hook 'ruby-mode-hook #'rubocopfmt-mode))

;;; ================ done ================

(provide 'my-ruby-mode)
