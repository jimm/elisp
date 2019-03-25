(defun -send-to-iterm (str suppress-newline)
  "Private method. Send STR to iTerm2, honoring SUPPRESS-NEWLINE."
  (let ((tempfile (concat "/tmp/emacs_terminal_"
                          (int-to-string (abs (random)))
                          ".txt")))
    (with-temp-file tempfile
      (insert str)
      (unless (or suppress-newline
                  (string-equal "\n" (substring str (1- (length str)))))
        (insert "\n")))
    (do-applescript (concat
                     "tell application \"" *my-terminal-program* "\"\n"
                     "	tell the current window\n"
                     "    tell the current session\n"
                     "      write contents of file \"" tempfile "\"\n"
                     "    end tell\n"
                     "	end tell\n"
                     "end tell\n"
                     ))
    (delete-file tempfile)))

(defun -send-to-terminal (str suppress-newline)
  "Private method. Send STR to Terminal, _ignoring_ SUPPRESS-NEWLINE."
    (do-applescript (concat
                     "tell application \"" *my-terminal-program* "\"\n"
                     "	do script \""
                     (replace-regexp-in-string "\"" "\"" str)
                     "\" in front window\n"
                     "end tell\n"
                     )))

(defun send-to-terminal (str &optional suppress-newline)
  "Send STR to the front window/session in *MY-TERMINAL-PROGRAM*,
with an additional newline if STR does not end in one (and if
SUPPRESS-NEWLINE is not nil). STR may contain multiple lines."
  (interactive "sTerminal input: ")
  (if (string-equal *my-terminal-program* "iTerm")
      (-send-to-iterm str suppress-newline)
    (-send-to-terminal str suppress-newline)))

(defun send-region-to-terminal ()
  "Send the region to *MY-TERMINAL-PROGRAM* using
send-to-terminal."
  (interactive)
  (send-to-terminal (buffer-substring-no-properties (point) (mark))))

(defun send-buffer-to-terminal ()
  "Send the current buffer to *MY-TERMINAL-PROGRAM* using
send-to-terminal."
  (interactive)
  (send-to-terminal (buffer-substring-no-properties (point-min) (point-max))))

(defun send-buffer-file-to-terminal (command)
  "Send COMMAND + the (properly quoted quoted) file name of the
current buffer to *MY-TERMINAL-PROGRAM* using send-to-terminal."
  (interactive "sCommand: ")
  (send-to-terminal (concat command " " (shell-quote-argument buffer-file-name))))

(defun send-current-line-to-terminal ()
  "Send the current line to *MY-TERMINAL-PROGRAM* using
`send-to-terminal'. See also
`send-current-line-to-terminal-and-next-line' which I have bound
to a key."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (push-mark)
    (forward-line)
    (send-region-to-terminal)))

(defun send-current-line-to-terminal-and-next-line ()
  "Send the current line to *MY-TERMINAL-PROGRAM* and move to the
next line. This is a nice function to have bound to a key
globally."
  (interactive)
  (send-current-line-to-terminal)
  (forward-line))

(defun switch-to-terminal ()
  "Make *MY-TERMINAL-PROGRAM* the front application."
  (interactive)
  (do-applescript (concat
                   "tell application \"" *my-terminal-program* "\"\n"
                   "  activate\n"
                   "end tell\n")))
