(defun open-email-client ()
  "Open the default email client."
  (interactive)
  (shell-command "open mailto:"))

;;; On Mac OS, GUI applications don't get your login environment variables.
;;; Read them from a shell and set them, except for a few, and set
;;; `exec-path' from the contents of PATH.

(defcustom *mac-ignore-env-var-list*
  '("_" "PS1" "PWD" "OLDPWD" "SHELL" "SHLVL" "RBENV_VERSION" "RBENV_DIR")
  "Environment variables to ignore.")

(defun mac-append-to-exec-path (path)
  "Add all the paths in the colon-separated PATH to `exec-path'."
  (mapc (lambda (path-element)
          (add-to-list 'exec-path path-element))
        (split-string val ":")))

(defun mac-process-env-string (setting)
  "Parse an environment variable SETTING of the form \"foo=bar\".

Call `setenv' to set the corresponding value in Emacs. If the
variable is \"PATH\", also call `mac-append-to-exec-path'."
  (let* ((idx (string-match "=" setting))
         (env (substring setting 0 idx))
         (val (substring setting (1+ idx))))
    (unless (member env *mac-ignore-env-var-list*)
      (setenv env val)
      (when (equal env "PATH")
        (mac-append-to-exec-path val)))))

(defun mac-load-environment-and-path ()
  "Read env from shell and set our environment and `exec-path'."
  (mapc #'mac-process-env-string
        (let ((envs
               (if (file-exists-p (concat user-emacs-directory "env"))
                   (with-temp-buffer
                     (insert-file-contents (concat user-emacs-directory "env"))
                     (buffer-string))
                 (shell-command-to-string "INSIDE_EMACS=1 ZDOTDIR=$HOME /bin/zsh -i -c /usr/bin/env"))))
          (cdr (reverse (split-string envs "\n"))))))

(mac-load-environment-and-path)

;;; ================================================================

(defvar *my-pim-dir* "~/pim/")

(setq *my-sql-regex* "^--.*"
      *my-sql-regex-replacement* ""
      Man-switches "-M /usr/share/man:/usr/local/share/man:/opt/local/share/man"
      browse-url-generic-program "open"
      dired-use-ls-dired nil
      dumb-jump-default-project "~/src"
      ;; Smoother mouse wheel scrolling
      mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control))))
      ns-command-modifier 'meta         ; define Command as Meta key
      ns-option-modifier 'super         ; define Option as Super key
      ns-right-command-modifier 'super  ; let's see if I use Right-Cmd Super
      sql-sqlite-program "sqlite3"
      epa-pinentry-mode 'loopback
      explicit-shell-file-name "/bin/zsh")

;;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=5683#19
(when (fboundp 'ns-list-colors)
  (setq x-colors (ns-list-colors)))     ; fix build bug in 23.4, also in 24.1
