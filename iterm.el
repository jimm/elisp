(defun send-to-iterm (str &optional suppress-newline)
  "Send STR to the front window/session in iTerm, with an
additional newline if STR does not end in one (and if
SUPPRESS-NEWLINE is not nil). STR may contain multiple lines."
  (interactive "siTerm input: ")
  (let ((tempfile (concat "/tmp/emacs_iterm_"
                          (int-to-string (abs (random)))
                          ".txt")))
    (with-temp-file tempfile
      (insert str)
      (unless (or suppress-newline
                  (string-equal "\n" (substring str (1- (length str)))))
        (insert "\n")))
    (do-applescript (concat
                     "tell application \"iTerm\"\n"
                     "	tell the current window\n"
                     "    tell the current session\n"
                     "      write contents of file \"" tempfile "\"\n"
                     "    end tell\n"
                     "	end tell\n"
                     "end tell\n"
                     ))
    (delete-file tempfile)))

(defun send-region-to-iterm ()
  "Send the region to iTerm using send-to-iterm."
  (interactive)
  (send-to-iterm (buffer-substring-no-properties (point) (mark))))

(defun send-buffer-to-iterm ()
  "Send the current buffer to iTerm using send-to-iterm."
  (interactive)
  (send-to-iterm (buffer-substring-no-properties (point-min) (point-max))))

(defun send-buffer-file-to-iterm (command)
  "Send COMMAND + the (properly quoted quoted) file name of the
current buffer to iTerm using send-to-iterm."
  (interactive "sCommand: ")
  (send-to-iterm (concat command " " (shell-quote-argument buffer-file-name))))

(defun send-current-line-to-iterm ()
  "Send the current line to iTerm using `send-to-iterm'. See also
`send-current-line-to-iterm-and-next-line' which I have bound to
a key."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (push-mark)
    (forward-line)
    (send-region-to-iterm)))

(defun send-current-line-to-iterm-and-next-line ()
  "Send the current line to iTerm and move to the next line. This
is a nice function to have bound to a key globally."
  (interactive)
  (send-current-line-to-iterm)
  (forward-line))

(defun switch-to-iterm ()
  "Make iTerm the front application."
  (interactive)
  (do-applescript (concat
                   "tell application \"iTerm\"\n"
                   "  activate\n"
                   "end tell\n")))
