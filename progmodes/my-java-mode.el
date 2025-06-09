(add-to-list 'auto-mode-alist '("\\.aj$" . java-mode)) ; Roo aspect files
(add-to-list 'auto-mode-alist '("\\.jsp$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.w[as]r$" . archive-mode))

(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 4
                  c-indent-level 4)
            (setq indent-tabs-mode nil)
            (c-set-style "java")))

(setq java-indent-level 4)

(defun upto (list elem)
  "Returns a list whose elements are those of LIST up to but not
including ELEM."
  (if (or (eq nil list) (equal (car list) elem))
      nil
    (cons (car list) (upto (cdr list) elem))))

(defun path-to-java-package (path)
  "Returns a Java package name for PATH, which is a file path.
Looks for 'src' or 'src/java' in PATH and uses everything after
that, turning slashes into dots. For example, the path
/home/foo/project/src/com/yoyodyne/project/Foo.java becomes
'com.yoyodyne.project'. If PATH is a directory, the last part of
the path is ignored. That is a bug, but it's one I can live with
for now."
  (interactive)
  (let ((reverse-path-list (cdr (reverse (split-string path "/")))))
    (mapconcat
     'identity
     (reverse (upto reverse-path-list
		    (if (member "java" reverse-path-list) "java" "src")))
     ".")))

(defun my-java-insert-package ()
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "package ") (kill-line 1))
    (insert "package " (path-to-java-package (buffer-file-name)) ";\n")))

(defun my-java-read-package ()
  "When run within a Java buffer, searches for the package name
and returns it."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^package +\\(.*\\);")
    (match-string-no-properties 1)))
