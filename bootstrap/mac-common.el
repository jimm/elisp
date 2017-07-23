;;; On Mac OS, GUI applications don't get your login environment variables.
;;; Read them from a shell and set them, except for a few, and set
;;; `exex-path' from the contents of PATH.

(defcustom ignore-env-var-list
  '("_" "PS1" "PWD" "OLDPWD" "SHELL" "SHLVL" "RBENV_VERSION" "RBENV_DIR")
  "Environment variables to ignore.")

(defun mac-append-to-exec-path (path)
  (mapc (lambda (path-element)
          (add-to-list 'exec-path path-element))
        (split-string val ":")))

(defun mac-process-env-string (setting)
  (let* ((idx (string-match "=" setting))
         (env (substring setting 0 idx))
         (val (substring setting (1+ idx))))
    (cond ((equal env "PATH")
           (setenv env val)
           (mac-append-to-exec-path val))
          ((member env ignore-env-var-list)
           nil)                   ; skip
          (t
           (setenv env val)))))

(defun mac-load-environment-and-path ()
  (mapc #'mac-process-env-string
        (let ((envs (shell-command-to-string "/bin/bash -l -c env")))
          (cdr (reverse (split-string envs "\n"))))))

(mac-load-environment-and-path)

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
      sql-sqlite-program "sqlite3")

(defun mode-line-visible-bell ()
  (unless (memq this-command
                '(isearch-abort
                  abort-recursive-edit
                  exit-minibuffer
                  mwheel-scroll
                  down up
                  next-line previous-line
                  backward-char forward-char))
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=5683#19
(when (fboundp 'ns-list-colors)
  (setq x-colors (ns-list-colors)))     ; fix build bug in 23.4, also in 24.1

;;; suspend-frame seems to crash Mac OS X Emacs
(put 'suspend-frame 'disabled t)
