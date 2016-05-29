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

(defun git-root-dir ()
  "Returns the current directory's root git repo directory."
  (file-name-directory (locate-dominating-file default-directory ".git")))

(defun git-root-dired ()
  "Runs dired in the current dir's root git repo directory."
  (interactive)
  (dired (git-root-dir)))

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
;; Git / Magit
;;
(setenv "GIT_PAGER" "cat")
(setq magit-last-seen-setup-instructions "1.4.0"
      magit-push-always-verify nil)

(defun git-revert ()
  "Checks out the current buffer's file in Git and reverts the current buffer."
  (interactive "*")
  (shell-command (concat "git checkout " (file-name-nondirectory (buffer-file-name))))
  (revert-buffer t t))

;;
;; fzf
;;
(when (fboundp #'fzf)
  (defun git-root-fzf ()
    (interactive)
    (fzf-directory (git-root-dir))))

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

;; Returns file(s) starting at default directory. See also
;; get-closest-pathname defined below (which does the same thing but assumes
;; root-directory is default-directory).
(defun find-up (fname-regexp root-directory)
  "Searches starting at ROOT-DIRECTORY for FNAME-REGEXP. If not
found, goes up until it hits the system root directory, looking
for FNAME-REGEXP."
  (interactive "sFilename regex: \nDSearch root directory: ")
  (let* ((dir (file-name-as-directory (expand-file-name root-directory)))
	 (dirname (directory-file-name dir))
	 (filter-regexp "\\.git\\|\\.svn\\|classes\\|build\\|target\\|TAGS\\|CVS\\|~")
	 (files
	  (remove-if
	   (lambda (f) (string-match filter-regexp f))
           (directory-files dirname t fname-regexp)))
	 (len (length files)))
    (cond ((zerop len)
	   ; (message "%s not found in %s" fname-regexp dir))
	   (cond ((equal "/" dirname) (message "%s not found" fname-regexp))
		 (t (find-up fname-regexp (file-name-directory dirname)))))
	  ((= 1 len) (car files))
	  (t files))))

;; See also find-up and the built-in function locate-dominating-file.
(defun get-closest-pathname (file)
  "Find the first instance of FILE at or above the current directory.
If it does not find FILE, then it returns the name of FILE in the
current directory, suitable for creation.

This may not do the correct thing in presence of links."
  (let* ((path (expand-file-name file))
         (f (file-name-nondirectory path))
         (d (locate-dominating-file path f)))
    (if d (concat d f)
      (expand-file-name f default-directory))))
