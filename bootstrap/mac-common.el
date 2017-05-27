;;; For each PATH element returned by launchctl, add it to exec-path if it's
;;; not already there. Also make sure it's set as an env var properly within
;;; Emacs.
(let ((true-path (shell-command-to-string "launchctl getenv \"PATH\"")))
  (mapc (lambda (path)
          (add-to-list 'exec-path path))
        (split-string true-path ":"))
  (setenv "PATH" true-path))

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
