;;; inf-sbt.el --- Run an SBT process in a buffer

;; Based on inf-ruby.el by Yukihiro Matsumoto and Nobuyoshi Nakada

;;; Commentary:
;;
;; inf-sbt.el provides a REPL buffer connected to an SBT subprocess.
;;

(require 'comint)
(require 'compile)
(require 'scala-mode2)

(defvar inf-sbt-default-implementation "sbt"
  "Which sbt implementation to use if none is specified.")

(defvar inf-sbt-prompt-pattern "^> *"
  "Prompt regex pattern of sbt interpreter.")

(defvar inf-sbt-mode-hook nil
  "*Hook for customising inf-sbt mode.")

(defvar inf-sbt-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    map)
  "*Mode map for inf-sbt-mode")

;; FIXME
(defconst inf-sbt-error-regexp-alist
  '(("SyntaxError: compile error\n^\\([^\(].*\\):\\([1-9][0-9]*\\):" 1 2)
    ("^\tfrom \\([^\(].*\\):\\([1-9][0-9]*\\)\\(:in `.*'\\)?$" 1 2)))

;;;###autoload
(defun inf-sbt-setup-keybindings ()
  "Set local key defs to invoke inf-sbt from scala-mode."
  (define-key scala-mode-map "\C-c\C-l" 'sbt-clean)
  (define-key scala-mode-map "\C-c\C-p" 'sbt-compile)
  (define-key scala-mode-map "\C-c\C-t" 'sbt-test)
  (define-key scala-mode-map "\C-c\C-r" 'sbt-run)
  (define-key scala-mode-map "\C-c\C-j" 'sbt-package)
  (define-key scala-mode-map "\C-c\C-m" 'sbt-send-command)
  (define-key scala-mode-map "\C-c\C-z" 'sbt-switch-to-inf))

(defvar inf-sbt-buffer nil "Current sbt process buffer.")

(defun inf-sbt-mode ()
  "Major mode for interacting with an inferior sbt process.

The following commands are available:
\\{inf-sbt-mode-map}

A sbt process can be fired up with M-x inf-sbt.

Customization: Entry to this mode runs the hooks on comint-mode-hook and
inf-sbt-mode-hook (in that order).

You can send commands to the inferior sbt process from other buffers
containing Scala source.
    sbt-switch-to-inf switches the current buffer to the sbt process buffer.
    sbt-clean sends the `clean' command to the sbt process.
    sbt-compile sends the `compile' command to the sbt process.
    sbt-test sends the `test' command to the sbt process.
    sbt-run sends the `run' command to the sbt process.
    sbt-package sends the `package' command to the sbt process.
    sbt-send-command prompts for text to send to the sbt process.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp inf-sbt-prompt-pattern)
  (setq major-mode 'inf-sbt-mode)
  (setq mode-name "Inf-SBT")
  (setq mode-line-process '(":%s"))
  (use-local-map inf-sbt-mode-map)
  (setq comint-get-old-input (function inf-sbt-get-old-input))
  (make-local-variable 'compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist inf-sbt-error-regexp-alist)
  (compilation-shell-minor-mode t)
  (run-hooks 'inf-sbt-mode-hook))

;; adapted from replace-in-string in XEmacs (subr.el)
(defun inf-sbt-remove-in-string (str regexp)
  "Remove all matches in STR for REGEXP and returns the new string."
  (let ((rtn-str "") (start 0) match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
            start (match-end 0)
            rtn-str (concat rtn-str (substring str prev-start match))))
    (concat rtn-str (substring str start))))

(defun inf-sbt-get-old-input ()
  "Snarf the sexp ending at point"
  (save-excursion
    (let ((end (point)))
      (re-search-backward inf-sbt-prompt-pattern)
      (inf-sbt-remove-in-string (buffer-substring (point) end)
                                 inf-sbt-prompt-pattern))))

;;;###autoload
(defun inf-sbt ()
  "Run an inferior sbt process in a buffer. Runs the
hooks `inf-sbt-mode-hook' \(after the `comint-mode-hook' is
run)."

  (interactive)
  (run-sbt))

;;;###autoload
(defun run-sbt ()
  "Run an inferior sbt process, input and output via buffer *sbt*.
If there is a process already running in `*sbt*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `sbt-program-name').  Runs the hooks `inferior-sbt-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive)

  (if (not (comint-check-proc inf-sbt-buffer))
      (let ((commandlist "sbt"))
        (set-buffer (make-comint "sbt" "sbt"))
        (inf-sbt-mode)))
  (pop-to-buffer (setq inf-sbt-buffer "*sbt*")))

(defun inf-sbt-proc ()
  "Returns the current SBT process. See variable inf-sbt-buffer."
  (or (get-buffer-process (if (eq major-mode 'inf-sbt-mode)
                              (current-buffer)
                            inf-sbt-buffer))
      (error "No current process. See variable inf-sbt-buffer")))

;; These commands are added to the sbt-mode keymap:

(defconst sbt-send-terminator "--inf-sbt-%x-%d-%d-%d--"
  "Template for sbt here document terminator.
Must not contain sbt meta characters.")

(defconst inf-sbt-eval-binding "SBT.conf[:MAIN_CONTEXT].workspace.binding")

(defconst sbt-eval-separator "")

(defun sbt-send-command (cmd)
  "Send CMD to the inferior sbt process."
  (interactive "sCommand: ")
  (save-excursion
    (let ((m (process-mark (inf-sbt-proc))))
      (set-buffer (marker-buffer m))
      (goto-char m)
      (set-marker m (point))))
  (comint-send-string (inf-sbt-proc) (concat cmd "\n")))

(defun sbt-clean ()
  (interactive)
  (sbt-send-command "clean"))

(defun sbt-compile ()
  (interactive)
  (sbt-send-command "compile"))

(defun sbt-test ()
  (interactive)
  (sbt-send-command "test"))

(defun sbt-run ()
  (interactive)
  (sbt-send-command "run"))

(defun sbt-send-region (start end)
  "Send the current region to the inferior Sbt process."
  (interactive "r")
  (let (term (file (or buffer-file-name (buffer-name))) line)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char start)
        (setq line (+ start (forward-line (- start)) 1))
        (goto-char start)
        (while (progn
                 (setq term (apply 'format sbt-send-terminator (random) (current-time)))
                 (re-search-forward (concat "^" (regexp-quote term) "$") end t)))))
    ;; compilation-parse-errors parses from second line.
    (save-excursion
      (let ((m (process-mark (inf-sbt-proc))))
        (set-buffer (marker-buffer m))
        (goto-char m)
        (insert sbt-eval-separator "\n")
        (set-marker m (point))))
    (comint-send-string (inf-sbt-proc) (format "eval <<'%s', %s, %S, %d\n"
                                                term inf-sbt-eval-binding
                                                file line))
    (comint-send-region (inf-sbt-proc) start end)
    (comint-send-string (inf-sbt-proc) (concat "\n" term "\n"))))

(defun sbt-send-last-sexp ()
  "Send the previous sexp to the inferior Sbt process."
  (interactive)
  (sbt-send-region (save-excursion (sbt-backward-sexp) (point)) (point)))

(defun sbt-switch-to-inf (eob-p)
  "Switch to the sbt process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer inf-sbt-buffer)
      (pop-to-buffer inf-sbt-buffer)
    (error "No current process buffer. See variable inf-sbt-buffer."))
  (cond (eob-p
         (push-mark)
         (goto-char (point-max)))))

(defun sbt-send-region-and-go (start end)
  "Send the current region to the inferior Sbt process.
Then switch to the process buffer."
  (interactive "r")
  (sbt-send-region start end)
  (sbt-switch-to-inf t))

(defun sbt-escape-single-quoted (str)
  (replace-regexp-in-string "'" "\\\\'"
    (replace-regexp-in-string "\n" "\\\\n"
      (replace-regexp-in-string "\\\\" "\\\\\\\\" str))))

(defsubst inf-sbt-fix-completions-on-windows ()
  "On Windows, the string received by `accept-process-output'
starts with the last line that was sent to the Sbt process.
The reason for this is unknown. Remove this line from `completions'."
  (if (eq system-type 'windows-nt)
    (setq completions (cdr completions))))

(defun inf-sbt-completions (seed)
  "Return a list of completions for the line of sbt code starting with SEED."
  (let* ((proc (get-buffer-process inf-sbt-buffer))
	 (comint-filt (process-filter proc))
	 (kept "") completions)
    (set-process-filter proc (lambda (proc string) (setq kept (concat kept string))))
    (process-send-string proc (format "puts SBT::InputCompletor::CompletionProc.call('%s').compact\n"
                                      (sbt-escape-single-quoted seed)))
    (while (and (not (string-match inf-sbt-prompt-pattern kept))
                (accept-process-output proc 2)))
    (setq completions (butlast (split-string kept "\r?\n") 2))
    (inf-sbt-fix-completions-on-windows)
    (set-process-filter proc comint-filt)
    completions))

(defun inf-sbt-completion-at-point ()
  (let* ((curr (replace-regexp-in-string "\n$" "" (thing-at-point 'line)))
         (completions (inf-sbt-completions curr)))
    (if completions
        (if (= (length completions) 1)
            (car completions)
          (completing-read "possible completions: "
                           completions nil t curr)))))

(defun inf-sbt-complete (command)
  "Complete the sbt code at point. Relies on the sbt/completion
Module used by readline when running sbt through a terminal"
  (interactive (list (inf-sbt-completion-at-point)))
  (when command
   (kill-whole-line 0)
   (insert command)))

(defun inf-sbt-complete-or-tab (&optional command)
  "Either complete the sbt code at point or call
`indent-for-tab-command' if no completion is available."
  (interactive (list (inf-sbt-completion-at-point)))
  (if (not command)
      (call-interactively 'indent-for-tab-command)
    (inf-sbt-complete command)))

;;;###autoload
(eval-after-load 'scala-mode2
  '(inf-sbt-setup-keybindings))

(provide 'inf-sbt)
;;; inf-sbt.el ends here
