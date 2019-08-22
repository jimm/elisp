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
    (find t (mapcar (lambda (f) (file-exists-p (concat dir f)))
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
               ".tox" "virtualenv" "venv" "__pycache__")
              grep-find-ignored-directories)

      grep-find-ignored-files
      (list
       "TAGS" "*.[wj]ar" "*.beam"
       "*.png" "*.gif" "*.jpg" "*.jpeg"
       ".#*" "*.o" "*~" "*.so" "*.a" "*.elc"
       "*.class" "*.lib" "*.lo" "*.la" "*.toc" "*.aux"
       "*.pyc" "*.pyo")

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

By default, initializes the search string with the current symbol
at point. With a prefix argument, reads the regex from the
minibuffer."
  (interactive "P")
  (let* ((symbol-at-point (thing-at-point 'symbol))
         (regexp (if (and arg (symbol-at-point))
                   (regexp-quote symbol-at-point)
                     (read-from-minibuffer
                      "Search regexp: " nil nil nil 'grep-find-history)))

         (default-directory (git-root-dir))
         (case-ignore-flag (and (isearch-no-upper-case-p regexp t) "-i"))
         (cmd (concat "git grep --extended-regexp --line-number --full-name"
                      " --untracked " case-ignore-flag " \"" regexp "\"")))
    (while (equal "" regexp)
        (setq regexp (read-from-minibuffer
                      "Search regexp (must not be the empty string): " nil nil nil 'grep-find-history)))
    (grep-find cmd)))

(defun git-grep-callers-python (arg)
  "Runs 'git grep \"[. \\t]current_symbol\\(\"' to find callers of the symbol
at point.

With a prefix argument, includes the symbol's definition. This is specific
to Python because we look for \"def current_symbol\"."
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
         (ignore-def-grep (concat " | grep -v 'def " symbol-at-point "'"))
         (cmd (concat "git grep --extended-regexp --line-number --full-name"
                      " --untracked " case-ignore-flag " '" regexp "'"
                      (unless arg ignore-def-grep))))
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
