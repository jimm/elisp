;;; My collection of functions and settings that are sort of like
;;; projectile-mode.

(defun makeup-dir-p (file-or-dir)
  "Returns `t' if `file-or-dir' is a directory from which we should run the
`makeup' shell command."
  (let ((dir (if (directory-name-p file-or-dir)
                 file-or-dir
               (file-name-nondirectory file-or-dir))))
    ;; This will be ever so slightly faster if you put more-frequently used
    ;; files nearer to the front of the list.
    (member t (mapcar (lambda (f) (file-exists-p (concat dir f)))
                      '("Makefile" "mix.exs" "shard.yml"  "tox.ini" "Rakefile"
                        "project.clj" "main.go" "pkg" "makefile" "pom.xml"
                        "Cargo.toml" "cargo.toml" "build.xml" "build.sbt")))))

(defun makeup (&optional args)
  "Finds the first build file in the default directory or any
directory above and then runs the appropriate build command,
passing on any args given to this script."
  (interactive "sMakeup args: ")
  (let* ((dir (locate-dominating-file default-directory #'makeup-dir-p))
         (default-directory (or dir default-directory)))
    (compile (concat "makeup " args))))

(setq compile-command "makeup "  ; script finds make/rake/mix.exs/build/etc.
      grep-command "grep -n ")

;; For rgrep, grep-find, and friends
(load "grep")
(setq grep-find-ignored-directories
      (append (list
               "tmp" "target" "ebin" "_build" "_site" ".vagrant" "node_modules"
               ".tox" "virtualenv" "venv" "__pycache__" "dist" "_dist"
               "__nugetartifacts")
              grep-find-ignored-directories)

      grep-find-ignored-files
      (list
       "TAGS" "*.[wj]ar" "*.beam"
       "*.png" "*.gif" "*.jpg" "*.jpeg"
       ".#*" "*.o" "*~" "*.so" "*.a" "*.elc"
       "*.class" "*.lib" "*.lo" "*.la" "*.toc" "*.aux"
       "*.pyc" "*.pyo" "*.dll")

      grep-find-use-xargs 'gnu)
;;
;; Git / Magit
;;
(setenv "GIT_PAGER" "cat")

(defun git-root-dir ()
  "Returns the current directory's root Git repo directory, or
NIL if the current directory is not in a Git repo."
  (let ((dir (locate-dominating-file default-directory ".git")))
    (when dir
      (file-name-directory dir))))

(defun git-root-dired ()
  "Runs dired in the current dir's root Git repo directory. If
not in a Git repo, uses the current directory."
  (interactive)
  (dired (or (git-root-dir) default-directory)))

(defun git-grep (arg)
  "Runs 'git grep', starting the search in the current
directory's root git repo directory.

By default, reads the regex from the minibuffer. With a prefix
ARG, initializes the search string with the current symbol at
point. If ARG is 16 (C-uC-u prefix), adds word boundary regex
markers before and after the current symbol.

Each line of output is truncated to a max of 240 characters."
  (interactive "P")
  (let* ((symbol-at-point (thing-at-point 'symbol))
         (regexp (if (and arg (symbol-at-point))
                     (concat (when (= (car arg) 16) "\\b")
                             (regexp-quote symbol-at-point)
                             (when (= (car arg) 16) "\\b"))
                   (read-from-minibuffer
                    "Search regexp: " nil nil nil 'grep-find-history)))
         (default-directory (git-root-dir))
         (case-ignore-flag (and (isearch-no-upper-case-p regexp t) "-i"))
         (cmd (concat "git grep --extended-regexp --line-number --full-name"
                      " --untracked --no-color " case-ignore-flag " -- '" regexp
                      "' | cut -c -240")))
    (while (equal "" regexp)
        (setq regexp (read-from-minibuffer
                      "Search regexp (must not be the empty string): " nil nil nil 'grep-find-history)))
    (grep-find cmd)))

(defun git-grep-callers-python-ruby (arg)
  "Runs 'git grep \"\\bcurrent_symbol\\b\"' to find callers of the symbol
at point.

With a prefix ARG, includes the symbol's definition. This is
specific to Python/Ruby because we look for \"def current_symbol\\b\"."
  (interactive "P")
  (let* ((symbol-at-point (thing-at-point 'symbol))
         (regexp (concat "\\b"
                         (or symbol-at-point
                             (read-from-minibuffer
                              "Symbol (must not be the empty string): "
                              nil nil nil 'grep-find-history))
                         "\\b"))
         (default-directory (git-root-dir))
         (case-ignore-flag (and (isearch-no-upper-case-p regexp t) "-i"))
         (ignore-def-grep (concat " | grep -v 'def " symbol-at-point "\\b'"))
         (cmd (concat "git grep --perl-regexp --line-number --full-name"
                      " --untracked " case-ignore-flag " '" regexp "'"
                      (unless arg ignore-def-grep)
                      (when (not (equal system-type 'windows-nt)) " | cut -c -240"))))
    (grep-find cmd)))

;;; ================================================================
;;; Finding files
;;;
;;; ...but see also `fzf'.
;;; ================================================================

;;
;; ef (find)
;; must come before loading my eshell customize
;;
(load "find-lisp")
(defun ef (find-name-arg root-directory &optional ignore)
  "Searches recursively in ROOT-DIRECTORY or current directory
for FIND-NAME-ARG. If one file is found, that file is opened. If
more than one is found, opens a dired buffer on the list of
files. If no files are found, continue searching up the directory
tree.

Ignores certain directories such as .git, .svn, and target. This
list is hard-coded, though it would be easy to make it an
optional argument."
  (interactive "sFilename regex: \nDSearch root directory: ")
  (let* ((dir (file-name-as-directory root-directory))
	 (dirname (directory-file-name dir))
	 (files
	  (cl-remove-if
	   (lambda (f) (string-match
                        "^\\(\\.git\\|\\.svn\\|classes\\|build\\|target\\|CVS\\)$\\|^~"
                        f))
	   (split-string (shell-command-to-string
			  (concat "find " dirname " -name " find-name-arg)))))
	 (len (length files)))
    (cond ((zerop len)
	   (cond ((equal "/" dirname) (message "%s not found" find-name-arg))
		 (t (ef find-name-arg (file-name-directory dirname)))))
	  ((= 1 len) (find-file (car files)))
	  (t (find-dired dir (concat "-name " find-name-arg))))))

;;; ================================================================
;;; Copying file paths to the clipboard
;;; ================================================================

(defun -path-to-clipboard-kill-ring (s include-line-at-point)
"Saves `s' to the kill ring and GUI clipboard, optionally
appending the current line number. Turns $HOME prefix into '~'.
Also outputs the path."
  (let* ((line-num (line-number-at-pos))
         (str (concat s (if include-line-at-point
                            (concat ":" (int-to-string line-num))
                          ""))))
    (string-to-clipboard (string-replace (getenv "HOME") "~" str))))

(defun path-to-clipboard-kill-ring (&optional arg)
  "Copies path to file visited by current buffer to the kill ring and GUI
clipboard. Returns filename.

With an ARG, append the line number at point. From
https://stackoverflow.com/questions/2416655/file-path-to-kill-ring-in-emacs"
  (interactive "p")
  (let ((filename (or (buffer-file-name) default-directory)))
    (when filename
      (-path-to-clipboard-kill-ring filename (> arg 1))
      filename)))

(defun path-from-git-root-to-clipboard-kill-ring (&optional arg)
  "Copies relative path of file visited by current buffer to the kill ring and
GUI clipboard. Returns filename.

With an ARG, append the line number at point."
  (interactive "p")
  (let ((absolute-path (or (buffer-file-name) default-directory)))
    (when absolute-path
      (let* ((git-root-dir (expand-file-name (locate-dominating-file absolute-path ".git")))
             (relative-path (substring absolute-path (length git-root-dir))))
        (when relative-path
          (-path-to-clipboard-kill-ring relative-path (> arg 1)))))))
