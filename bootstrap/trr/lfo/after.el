(load-theme 'jim-light)

(setq user-email-address "jim.menard@therealreal.com"
      dired-use-ls-dired nil
      browse-url-generic-program "open"
      Man-switches "-M /usr/share/man:/opt/homebrew/share/man"
      sql-sqlite-program "sqlite3"
      rubocopfmt-rubocop-command "rubocop-daemon-wrapper"
      my-shell #'shell
      my-alternate-shell #'eshell)

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
