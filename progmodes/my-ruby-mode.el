(require 'rails-find-other-file)
(require 'rdoc-mode)

;; Use "M-x run-ruby" to start inf-ruby.
(autoload 'ruby-mode "ruby-mode" "Ruby mode" t nil)
(autoload 'run-ruby "inf-ruby" "Ruby inferior process (irb)" t nil)
(defalias  'inf-ruby 'run-ruby)
(defalias  'inferior-ruby 'run-ruby)
(defalias 'irb 'run-ruby)
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.r\\(b\\(w\\|x\\)?\\|html?\\|js\\)$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\([Rr]ake\\|[Cc]ap\\)file" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gem\\(spec\\)?$" . ruby-mode))
;; (setq ruby-indent-tabs-mode t)

(defun rails-root-p (path)
  "Returns true if path is a Rails root directory. Uses a heuristic that
involves looking for known Rails directories."
  (and path
       (file-directory-p path)
       (file-directory-p (concat path "app"))
       (file-directory-p (concat path "test"))
       (file-directory-p (concat path "config"))
       (file-directory-p (concat path "components"))
       (file-directory-p (concat path "log"))))

(defun parent-dir (path)
  "Returns the parent directory of path. Handles trailing slashes."
  (file-name-directory (cond ((equal "/" (substring path -1))
			      (substring path 0 -1))
			     (t path))))

(defun find-rails-root (path)
  "Returns Rails root dir at or above path. Returns nil if path is nil or no
Rails root dir is found."
    (message path)
    (cond ((not path) nil)
	  ((rails-root-p path) path)
	  ((equal "/" path) nil)
	  (t (find-rails-root (parent-dir path)))))

; Ever try to use M-q to reformat a paragraph in a comment? Ever notice how
; it will also reformat any following code into the paragraph as well? Put
; the following into your .emacs file to allow you to reformat the comment
; without messing up your code. -- JimWeirich
; From http://www.rubygarden.org/ruby?EmacsExtensions

(defvar jw-rb-para-begin-re "\\(^\\s-*#*\\s-*$\\)\\|\\(^\\s-*[^# ]\\)")

(defun jw-rb-goto-para-begin ()
  (search-backward-regexp jw-rb-para-begin-re)
  (beginning-of-line)
  (forward-line 1) )

(defun jw-rb-goto-para-end ()
  (search-forward-regexp jw-rb-para-begin-re)
  (beginning-of-line) )

(defun jw-rb-fill-comment-region ()
  (interactive "*")
  (save-excursion
    (jw-rb-goto-para-begin)
    (let ((start (point)))
      (jw-rb-goto-para-end)
      (narrow-to-region start (point))
      (fill-region start (point))
      (widen))))

(defun run-ruby-buffer ()
  (interactive)
  (let ((fn (buffer-file-name)))
    (if (file-exists-p fn)
        (progn
          (save-buffer)
          (compile (concat "ruby " (buffer-file-name))))
      (progn
        (let ((tmpfile (make-temp-file "ruby-region-" nil ".rb")))
          (write-region nil nil tmpfile)
          (compile (concat "ruby " tmpfile)))))))
;; Can't remove temp file because compile is async and file could be deleted
;; before it is used by compile.

(defun insert-ruby-hash-arrow ()
  (interactive "*")
  (insert "=>")
  (backward-char 2)
  (just-one-space)
  (forward-char 2)
  (just-one-space))

(defun run-ruby-test (test-name)
  "Will run TEST-NAME from the current buffer's file, which is
presumed to be a test file. If TEST-NAME is empty or nil, runs
all tests in the file."
  (interactive "sTest name (empty for all tests in the file): ")
  (let ((root-dir (locate-dominating-file (file-name-directory (buffer-file-name)) "Rakefile")))
    (if root-dir
        (let ((root-relative-file (substring (buffer-file-name) (length (expand-file-name root-dir)))))
          (progn
            (save-buffer)
            (compile (concat "cd " root-dir " && ruby -I test " root-relative-file
                             (when (> (length test-name) 0)
                               (concat " -n " test-name))))))
      (error "Can not find RAILS_ROOT (Rakefile not found)"))))

(setq ruby-mode-hook
      '(lambda ()
	 (define-key ruby-mode-map "\r" 'newline-and-indent)
	 (define-key ruby-mode-map "\M-\C-h" 'backward-kill-word)
	 (define-key ruby-mode-map "\M-q" 'jw-rb-fill-comment-region)
	 (define-key ruby-mode-map "\C-cr" 'run-ruby-buffer)
	 (define-key ruby-mode-map "\C-cd" 'debug-comment)
	 (define-key ruby-mode-map "\C-ch" 'insert-ruby-hash-arrow)
	 (define-key ruby-mode-map "\C-ct" 'run-ruby-test)
;; 	 (setq c-tab-always-indent nil)
 	 (setq ruby-indent-level 2)
	 (font-lock-mode 1)))

(defvar *rails-default-port* 3000)

;; ================================================================
;; Mongrel
;; ================================================================
(defun mongrel-start (dir &optional port)
  "Start mongrel_rails daemon from directory DIR on the specified
PORT (default *rails-default-port*)."
  (interactive (list
                (expand-file-name (read-directory-name "RAILS_ROOT: " nil default-directory t))
                (read-number "Port: " *rails-default-port*)))
  (shell-command (concat "cd " dir " && mongrel_rails start -d -p " (int-to-string (or port *rails-default-port*)))))

(defun mongrel-stop (dir)
  "Stop mongrel_rails daemon running in directory DIR."
  (interactive "DRAILS_ROOT: ")
  (shell-command (concat "cd " dir " && mongrel_rails stop")))

(defun mongrel-restart (dir &optional port)
  "Restart mongrel_rails daemon from directory DIR on the
specified PORT (default *rails-default-port*)."
  (interactive (list
                (expand-file-name (read-directory-name "RAILS_ROOT: " nil default-directory t))
                (read-number "Port: " *rails-default-port*)))
  (mongrel-stop dir)
  (mongrel-start dir (or port *rails-default-port*)))

;; ================================================================
;; Thin
;; ================================================================
(defun thin-start (dir &optional port)
  "Start thin daemon from directory DIR on the specified
PORT (default *rails-default-port*)."
  (interactive (list
                (expand-file-name (read-directory-name "RAILS_ROOT: " nil default-directory t))
                (read-number "Port: " *rails-default-port*)))
  (shell-command (concat "cd " dir " && thin start --daemonize -p " (int-to-string (or port *rails-default-port*)))))

(defun thin-stop (dir)
  "Stop thin daemon running in directory DIR."
  (interactive "DRAILS_ROOT: ")
  (shell-command (concat "cd " dir " && thin stop")))

(defun thin-restart (dir &optional port)
  "Restart thin daemon from directory DIR on the
specified PORT (default *rails-default-port*)."
  (interactive (list
                (expand-file-name (read-directory-name "RAILS_ROOT: " nil default-directory t))
                (read-number "Port: " *rails-default-port*)))
  (thin-stop dir)
  (thin-start dir (or port *rails-default-port*)))

;; ================================================================
;; Built-in Rails server script
;; ================================================================

(defun rails-server-start (&optional dir)
  "Start script/server from directory DIR."
  (interactive "DRAILS_ROOT: ")
  (shell-command (concat "cd " dir " && script/server &")
                 "*rails-server*"))

;; ================================================================
;; Gem server
;; ================================================================

(defun gem-server ()
  (interactive)
  (shell-command "gem server &" "*gem-server*")
  (bury-buffer (get-buffer "*gem-server*"))) ; doesn't hide buffer

(defun gem-server-stop ()
  (interactive)
  (interrupt-process (get-buffer "*gem-server*")))
