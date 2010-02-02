(require 'rails-find-other-file)
(require 'rdoc-mode)

;; Use "M-x run-ruby" to start inf-ruby.
(autoload 'ruby-mode "ruby-mode" "Ruby mode" t nil)
(autoload 'run-ruby "run-ruby" "Ruby inferior process (irb)" t nil)
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.r\\(b\\(w\\|x\\)?\\|html?\\|js\\)$" . ruby-mode))
(add-to-list 'auto-mode-alist '("^\\([Rr]ake\\|[Cc]ap\\)file" . ruby-mode))
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
  (next-line 1) )

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
  (save-buffer)
  (compile (concat "ruby " (buffer-file-name))))

(defun insert-ruby-hash-arrow ()
  (interactive "*")
  (insert "=>")
  (backward-char 2)
  (just-one-space)
  (forward-char 2)
  (just-one-space))

(setq ruby-mode-hook
      '(lambda ()
	 (define-key ruby-mode-map "\r" 'newline-and-indent)
	 (define-key ruby-mode-map "\M-\C-h" 'backward-kill-word)
	 (define-key ruby-mode-map "\M-q" 'jw-rb-fill-comment-region)
	 (define-key ruby-mode-map "\C-cr" 'run-ruby-buffer)
	 (define-key ruby-mode-map "\C-cd" 'debug-comment)
	 (define-key ruby-mode-map "\C-ch" 'insert-ruby-hash-arrow)
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
