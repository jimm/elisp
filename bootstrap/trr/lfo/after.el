;; -*- lexical-binding: t; -*-

(load-theme 'jim-light)

(setq user-email-address "jim.menard@therealreal.com"
      dired-use-ls-dired nil
      browse-url-generic-program "open"
      Man-switches "-M /usr/share/man:/opt/homebrew/share/man"
      sql-sqlite-program "sqlite3"
      rubocopfmt-rubocop-command "rubocop-daemon-wrapper" ; overridden in $trr/.dir-locals.el
      my-shell #'shell
      my-alternate-shell #'eshell)

;; Salesforce. Java isn't right, but it's close enough.
(add-to-list 'auto-mode-alist '("\\.cls$" . java-mode))

(defun trr-path (&optional arg)
  "Copies relative path of file visited by current buffer from parent dir
~/src/trr to the kill ring and GUI clipboard. Returns relative path.

With an ARG, append the line number at point."
  (interactive "p")
  (let ((absolute-path (or (buffer-file-name) default-directory)))
    (when absolute-path
      (let* ((trr-root-dir "/Users/jim.menard/src/trr/")
             (relative-path (substring absolute-path (length trr-root-dir))))
        (when relative-path
          (-path-to-clipboard-kill-ring relative-path (> (or arg 1) 1)))))))

;; Functions for visiting a $box/{docs,plans,prompts} file or directory when
;; visiting one of those other directories or a file in it.

;; FIXME
(defun my-other-box-dir (other-dir-name)
  (interactive)                         ; interactive for testing
  (let ((box-prefix (abbreviate-file-name (concat (getenv "box") "/"))))
    (unless (string-prefix-p box-prefix (abbreviate-file-name default-directory))
      (error (concat "default-directory " default-directory " is not a $box directory")))
    (let* ((here-remainder (substring-no-properties default-directory (length box-prefix))) ; "docs/a/"
           (parts (string-split here-remainder "/"))      ; ("a")
           (other-path-parts (cons other-dir-name (cdr parts))) ; ("other-dir-name" "a")
           (target-dir (concat box-prefix (string-join other-path-parts "/"))))
      (let ((maybe-buffer-file-path
             (when (buffer-file-name)
               (concat target-dir (file-name-nondirectory (buffer-file-name))))))
        (if (and maybe-buffer-file-path
                 (file-exists-p maybe-buffer-file-path))
            maybe-buffer-file-path
          target-dir)))))

(defun doc ()
  "Jump to the corresponding $box/docs file or directory."
  (interactive)
  (find-file (my-other-box-dir "docs")))

(defun prompt ()
  "Jump to the corresponding $box/prompts file or directory."
  (interactive)
  (find-file (my-other-box-dir "prompts")))

(defun plan ()
  "Jump to the corresponding $box/plans file or directory."
  (interactive)
  (find-file (my-other-box-dir "plans")))
