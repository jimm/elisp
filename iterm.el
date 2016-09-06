(defun send-to-iterm (str)
  "Send STR to the front window/session in iTerm. STR may contain
multiple lines separated by `\n'."
  (interactive "siTerm input: ")
  (let ((lines (split-string
                (replace-regexp-in-string "\"" "\\\"" str t t)
                "\n")))
    (do-applescript (concat
                     "tell application \"iTerm\"\n"
                     "	tell the current window\n"
                     "    tell the current session\n"
                     ;; could also use "write contents of file <foo>"
                     (mapconcat (lambda (s) (concat "write text \"" s "\"\n")) lines "")
                     "    end tell\n"
                     "	end tell\n"
                     "end tell\n"
                     ))))

(defun send-region-to-iterm ()
  "Send the region to iTerm using send-to-iterm."
  (interactive)
  (send-to-iterm (buffer-substring-no-properties (point) (mark))))

(defun send-buffer-to-iterm ()
  "Send the current buffer to iTerm using send-to-iterm."
  (interactive)
  (send-to-iterm (buffer-substring-no-properties (point-min) (point-max))))

(defun send-current-line-to-iterm ()
  "Send the current line to iTerm using `send-to-iterm'. See also
`send-current-line-to-iterm-and-next-line' which I have bound to
a key."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (push-mark)
    (end-of-line)
    (send-region-to-iterm)))

(defun send-current-line-to-iterm-and-next-line ()
  "Send the current line to iTerm and move to the next line. This
is a nice function to have bound to a key globally."
  (interactive)
  (send-current-line-to-iterm)
  (forward-line))
