; Most of this comes from http://www.khngai.com/emacs/cygwin.php

; path
(setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))
(setq exec-path (cons "c:/cygwin/bin/" exec-path))
(require 'cygwin-mount)
(cygwin-mount-activate)

; Gzip/gunzip
(setq archive-zip-use-pkzip nil)

(require 'dired-aux)

(defun dired-call-process (program discard &rest arguments)
  ;; 09Feb02, sailor overwrite this function because Gnu Emacs cannot
  ;; recognize gunzip is a symbolic link to gzip. Thus, if the program
  ;; is "gunzip", replace it with "gzip" and add an option "-d".

  ;; "Run PROGRAM with output to current buffer unless DISCARD is t.
  ;; Remaining arguments are strings passed as command arguments to PROGRAM."
  ;; Look for a handler for default-directory in case it is a remote file name.
  (let ((handler
         (find-file-name-handler (directory-file-name default-directory)
                                 'dired-call-process)))
    (if handler (apply handler 'dired-call-process
                       program discard arguments)
      (progn
        (if (string-equal program "gunzip")
            (progn
              (setq program "gzip")
              (add-to-list 'arguments "-d")
              )
            )
        (apply 'call-process program nil (not discard) nil arguments)
        )
      )))

; Bash
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt nil t)
(setq explicit-shell-file-name "bash.exe")
;; For subprocesses invoked via the shell
;; (e.g., "shell -c command")
(setq shell-file-name explicit-shell-file-name)
