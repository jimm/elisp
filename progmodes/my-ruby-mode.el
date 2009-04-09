(require 'rails-find-other-file)
(require 'rdoc-mode)

;; Use "M-x run-ruby" to start inf-ruby.
(autoload 'ruby-mode "ruby-mode" "Ruby mode" t nil)
(autoload 'run-ruby "run-ruby" "Ruby inferior process (irb)" t nil)
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.r\\(b\\(w\\|x\\)?\\|html?\\|js\\)$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\([Rr]ake\\|[Cc]ap\\)file$" . ruby-mode))
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

(defun mongrel-start (&optional dir port)
  "Start mongrel_rails daemon from directory dir on the specified port."
  (interactive "DRAILS_ROOT: 
nPort: ")
    (shell-command (concat "cd " dir " && mongrel_rails start -d -p "
			   (int-to-string port))))

(defun mongrel-stop (&optional dir)
  "Stop mongrel_rails daemon running in directory dir."
  (interactive "DRAILS_ROOT: ")
    (shell-command (concat "cd " dir " && mongrel_rails stop")))

(defun gem-server ()
  (interactive)
  (shell-command "gem_server &" "*gem-server*")
  (bury-buffer (get-buffer "*gem-server*"))) ; doesn't hide buffer

(defun gem-server-stop ()
  (interactive)
  (interrupt-process (get-buffer "*gem-server*")))