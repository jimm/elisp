;; Read $HOME/.environment and set env vars. Makes massive assumptions about
;; contents of ~/.environment. For example, assumes that no quote escaping
;; is necessary.
(let ((lines (with-temp-buffer
               (insert-file-contents (concat (getenv "HOME") "/Documents/src/sandbox/dotfiles/.environment"))
               (split-string (buffer-string) "\n" t))))
  (mapc (lambda (line)
          (when (equal "export" (substring line 0 6))
            (let* ((keyval (replace-regexp-in-string "^export *" "" line))
                   (k-n-v (split-string keyval "=" t))
                   (key (car k-n-v))
                   (val (substitute-in-file-name (cadr k-n-v))))
              (when val
                (setenv key val)))))
          lines))

(setq bookmark-default-file "c:/Users/jamenard/.emacs.d/bookmarks")

;;; ================================================================
;;; Read $PATH from bash login shell and add each element to exec path.
;;; Munge them a bit in the process.

(defun bash-paths ()
  "Return a list containing all the path elements defined by a
login bash shell."
  (split-string
   (shell-command-to-string "bash -l -c 'echo -n $PATH'")
   ":" t))

(defun fix-cygwin-path (path)
  "Given a PATH, return a copy suitable for Emacs."
  (cond ((or (equal "." (substring path 0 1))
             (equal "c:" (substring path 0 2)))
         path)
        ((equal "/c/" (substring path 0 3))
         (concat "c:" (substring path 2)))
        (t
         (concat "c:/cygwin64" path))))

(defvar *git-shell-bin-dir* "c:/Program Files (x86)/Git/bin")

;;; Set exec-path. Note: setting the "PATH" env var seems to screw up
;;; $EMACSHOME/bin/cmdproxy.exe.
(let* ((paths (mapcar #'fix-cygwin-path (bash-paths))))
  (add-to-list 'paths *git-shell-bin-dir*)
  (setq exec-path paths))
  ;; (mapc (lambda (p) (message p)) paths)
  ;; (setenv "PATH" (mapconcat (lambda (p) (concat "\"" p "\"")) paths ":"))

;;; ================================================================

(let* ((more-bad-names (or *more-grep-find-bad-names* ()))
       (bad-names (append (list "*.log" ".git" "TAGS" "*~" "*.class"
                                "*.[wj]ar" "target" "javadoc" "bytecode"
                                "*.o" "*.pyc" ".idea" "_build")
                          more-bad-names))
       (gfc (concat "c:\\cygwin64\\bin\\find.exe . \\( -name "
                    (mapconcat #'shell-quote-argument bad-names " -o -name ")
                    " \\) -prune -o -type f -print0 | xargs -0 c:\\cygwin64\\bin\\grep.exe -H -n ")))
  (setq grep-find-command (cons gfc (+ 1 (length gfc)))))

;;; ================================================================

(defun ensure-todays-date ()
  "Used by status function. Assumes point is at beginning of
  status file."
  (interactive)                         ; DEBUG
  (let ((date-header (concat "* " (format-time-string "%Y-%m-%d"))))
    (goto-char (point-min))
    (unless (looking-at (concat "* " date-header))
      (insert date-header "\n\n"))))

(defun status (status-str)
  "Insert a STATUS-STR into my status.org Org file."
  (interactive "sStatus: ")
  (find-file (substitute-in-file-name "$poa/status.org"))
  (ensure-todays-date)
  (goto-char (point-min))
  (forward-line)
  (insert "\n" status-str "\n"))

;;; ================================================================

(menu-bar-mode 0)
(display-time)
(setq browse-url-generic-program "explore") ; doesn't work :-(

(setq sql-sqlite-program "sqlite3")

(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)$" . markdown-mode))

;; Start Emacs server
(server-start)

(defmacro set-org-file-key (key file)
  "Map a KEY globally to one of my Org FILEs."
  `(global-set-key ,key
     (lambda ()
       (interactive)
       (find-file (concat *my-pim-dir* "orgs/" ,file)))))

(set-org-file-key [f4] "work/aig/todo.org")
(set-org-file-key [\C-f4] "todo.org")
(set-org-file-key [f6] "work/aig/notes.org")
(set-org-file-key [\C-f6] "notes.org")
