;;; My collection of functions that are sort of like projectile-mode.

(setq compile-command "makeup "  ; script finds make/rake/mix.exs/build/etc.
      grep-command "grep -n ")

;; For rgrep, grep-find, and friends
(load "grep")
(setq grep-find-ignored-directories
      (append (list "tmp" "target" "ebin" "_build" "_site")
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
  "Runs 'git grep' after reading the search regular expression
from the minibuffer. Starts the search in the current directory's
root git repo directory.

With a prefix argument, initializes the search string with the
current symbol at point."
  (interactive "P")
  (let* ((regexp (read-from-minibuffer
                  "Search regexp: "
                  (and arg (regexp-quote (thing-at-point 'symbol)))
                  nil nil 'grep-find-history))
         (default-directory (git-root-dir))
         (case-ignore-flag (and (isearch-no-upper-case-p regexp t) "-i"))
         (cmd (concat "git grep -E -n --full-name " case-ignore-flag
                      " \"" regexp "\"")))
  (grep-find cmd)))


;;
;; fzf
;;
(when (fboundp #'fzf)
  (defun git-root-fzf ()
    "Runs fzf from the Git root directory of the current buffer.
If the current buffer is not in a Git repo, runs fzf from the
current directory."
    (interactive)
    (fzf-directory (or (git-root-dir) default-directory))))

;;; ================================================================
;;; Finding files
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
