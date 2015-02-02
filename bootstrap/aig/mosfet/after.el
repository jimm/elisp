;; Read my environment setup file and set env vars. Makes massive
;; assumptions about contents of that file. For example, assumes that no
;; quote escaping is necessary.
(let ((lines (with-temp-buffer
               (insert-file-contents (concat (getenv "HOME") "/Documents/src/sandbox/dotfiles/environment"))
               (split-string (buffer-string) "\n" t))))
  (mapc (lambda (line)
          (when (and
                 (> (length line) 8)    ; "export x"
                 (equal "export" (substring line 0 6)))
            (let* ((keyval (replace-regexp-in-string "^export *" "" line))
                   (k-n-v (split-string keyval "=" t))
                   (key (car k-n-v))
                   (val (substitute-in-file-name (cadr k-n-v))))
              (when val
                (setenv key val)))))
          lines))

(setq bookmark-default-file "c:/Users/jamenard/.emacs.d/bookmarks")
(set-default-coding-systems 'utf-8-unix)

(require 'grep)
(grep-apply-setting 'grep-use-null-device nil)

;;; ================================================================
;;; Read $PATH from bash login shell and add each element to exec path.
;;; Munge them a bit in the process.

(defun bash-paths ()
  "Return a list containing all the path elements defined by a
login bash shell."
  (split-string
   (shell-command-to-string "c:/cygwin64/bin/bash -l -c 'echo -n $PATH'")
   ":" t))

(defun fix-cygwin-path (path)
  "Given a PATH, return a copy suitable for Emacs."
  (interactive)
  (cond ((or (equal "." (substring path 0 1))
             (equal "c:" (substring path 0 2)))
         path)
        ((equal "/c/" (substring path 0 3))
         (concat "c:" (substring path 2)))
        (t
         (concat "c:/cygwin64" path))))

;;; Set PATH, eshell-path-env, and exec-path.
(let* ((paths (mapcar #'fix-cygwin-path (bash-paths)))
       (path-string (mapconcat #'identity paths ";")))
  (setenv "PATH" path-string)
  (setq eshell-path-env path-string)
  (setq exec-path paths))

;;; ================================================================

(load "status")
(setq *status-file* (substitute-in-file-name "$poa/status.org"))

(menu-bar-mode 0)
(display-time)
(setq browse-url-generic-program "explore") ; doesn't work :-(

(setq sql-sqlite-program "sqlite3")

(add-to-list 'webjump-sites
             '("slack" . "https://aig-science-dev.slack.com/"))
(add-to-list 'webjump-sites
             '("todo" . "https://en.todoist.com/"))

;; Start Emacs server
(server-start)

(set-org-file-key [f6] "work/aig/notes.org")
(set-org-file-key [\C-f6] "notes.org")

(global-set-key [f5] #'status)

(when-fboundp-global-set-key "\C-xo" switch-window)
(when-fboundp-global-set-key [f11]   switch-window)
