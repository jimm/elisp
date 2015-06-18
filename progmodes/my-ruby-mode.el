(require 'my-rails)
(require 'rdoc-mode)
(require 'ruby-testing)

(defun insert-ruby-hash-arrow ()
  (interactive "*")
  (insert "=>")
  (backward-char 2)
  (just-one-space)
  (forward-char 2)
  (just-one-space))

(defun find-ruby-project-root (path)
  "Returns Ruby project root dir at or above PATH. Returns nil if
PATH is nil or no project root dir is found."
  (locate-dominating-file
   path
   (lambda (path)
     (or (rails-root-p (path))
         (file-exists-p (concat path "Rakefile"))
         (file-directory-p (concat path ".git/"))
         (file-directory-p (concat path ".svn/"))))))

(defun my-ruby-find-definition (word &optional start-dir)
  "Searches for \"def word_under_point\" from the root dir of the
current project."
  (interactive "sFind definition of: \nDStarting directory: ")
  (let ((root-dir (or start-dir (find-ruby-project-root (buffer-file-name)))))
    (when root-dir
      (rgrep (concat "def " word "[^_a-zA-Z0-9?!]") "*.rb" root-dir))))

(defun my-ruby-find-definition-at-point ()
  (interactive)
  (let* ((word-region (bounds-of-thing-at-point 'word))
         (end-point (cdr word-region))
         (next-char-str (buffer-substring-no-properties
                         end-point (+ 1 end-point)))
         (word (buffer-substring-no-properties
                (car word-region)
                (if (or (equal next-char-str "!") (equal next-char-str "?"))
                    (+ end-point 1)
                  end-point))))
    (my-ruby-find-definition word)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map "\r" #'newline-and-indent)
            (define-key ruby-mode-map "\M-\C-h" #'backward-kill-word)
            (define-key ruby-mode-map "\C-cr" #'executable-interpret)
            (define-key ruby-mode-map "\C-cd" #'debug-comment)
            (define-key ruby-mode-map "\C-ch" #'insert-ruby-hash-arrow)
            (define-key ruby-mode-map "\C-ct" #'run-ruby-test)
            (define-key ruby-mode-map "\C-cs" #'run-ruby-spec)
            (define-key ruby-mode-map "\C-c." #'my-ruby-find-definition-at-point)
            (define-key ruby-mode-map "{" #'self-insert-command)
            (define-key ruby-mode-map "}" #'self-insert-command)
            (setq ruby-indent-level 2)
            (font-lock-mode 1)))

(provide 'my-ruby-mode)
