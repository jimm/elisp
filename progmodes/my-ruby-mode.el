(require 'rails-find-other-file)
(require 'rdoc-mode)

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
all tests in the file.

If *ruby-test-inject-command* is defined it is run after changing
to the root dir and before running the test. For example, you can
use this to delete the log/test.log file."
  (interactive "sTest name (empty for all tests in the file): ")
  (let ((root-dir (locate-dominating-file (file-name-directory (buffer-file-name)) "Rakefile")))
    (if root-dir
        (let ((root-relative-file (substring (buffer-file-name) (length (expand-file-name root-dir)))))
          (progn
            (save-buffer)
            (compile (concat "cd " (shell-quote-argument (expand-file-name root-dir))
                             (when (boundp '*ruby-test-inject-command*)
                               (concat " && " *ruby-test-inject-command*))
                             " && ruby -I test " root-relative-file
                             (when (> (length test-name) 0)
                               (concat " -n " test-name))))))
      (error "Can not find RAILS_ROOT (Rakefile not found)"))))

(defun run-ruby-spec (spec-name-fragment)
  "Will run all specs whose full names include SPEC-NAME-FRAGMENT
from the current buffer's file, which is presumed to be an RSpec
test file. If SPEC-NAME-FRAGMENT is empty or nil, runs all tests
in the file.

If *ruby-test-inject-command* is defined it is run after changing
to the root dir and before running the test. For example, you can
use this to delete the log/test.log file."
  (interactive "sSpec name fragment (empty for all tests in the file): ")
  (let ((root-dir (locate-dominating-file (file-name-directory (buffer-file-name)) "Rakefile")))
    (if root-dir
        (let ((root-relative-file (substring (buffer-file-name) (length (expand-file-name root-dir)))))
          (progn
            (save-buffer)
            (compile (concat "cd " (shell-quote-argument (expand-file-name root-dir))
                             (when (boundp '*ruby-test-inject-command*)
                               (concat " && " *ruby-test-inject-command*))
                             " && rspec " root-relative-file
                             (when (> (length spec-name-fragment) 0)
                               (concat " -e " (shell-quote-argument spec-name-fragment)))))))
      (error "Can not find RAILS_ROOT (Rakefile not found)"))))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (define-key ruby-mode-map "\r" 'newline-and-indent)
             (define-key ruby-mode-map "\M-\C-h" 'backward-kill-word)
             (define-key ruby-mode-map "\M-q" 'jw-rb-fill-comment-region)
             (define-key ruby-mode-map "\C-cr" 'run-ruby-buffer)
             (define-key ruby-mode-map "\C-cd" 'debug-comment)
             (define-key ruby-mode-map "\C-ch" 'insert-ruby-hash-arrow)
             (define-key ruby-mode-map "\C-ct" 'run-ruby-test)
             (define-key ruby-mode-map "\C-cs" 'run-ruby-spec)
             (define-key ruby-mode-map "{" 'self-insert-command)
             (define-key ruby-mode-map "}" 'self-insert-command)
             (setq ruby-indent-level 2)
             (font-lock-mode 1)))
