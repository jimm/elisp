;; ================================================================
;; Status
;; ================================================================

(defun status-to-phone ()
  "Moves most recent two days' entries from *status-file* into a
Dropbox file that I can read from my phone. Also copies it into
the system paste buffer, ready for Slack. Useful for daily
standup meetings.

Assumes `*status-file*' is defined."
  (interactive)

  ;; Grab last two days' entries.
  (find-file *status-file*)
  (goto-char (point-min))
  (org-forward-heading-same-level 2)
  (copy-region-as-kill (point-min) (point))
  (goto-char (point-min))

  ;; Open file, copy yanked text
  (find-file (concat (getenv "dbox") "/Miscellaneous/status.txt"))
  (delete-region (point-min) (point-max))
  (goto-char (point-min))
  (yank)

  ;; Change headings to "Today" and "Yesterday" and swap their order
  (goto-char (point-min))
  (kill-line)
  (insert "*~~~~~~~~ Today ~~~~~~~~*")
  (search-forward-regexp "\n* [[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}")
  (beginning-of-line)
  (kill-line)
  (insert "*~~~~~~~~ Yesterday ~~~~~~~~*")
  (beginning-of-line)
  (kill-region (point) (point-max))
  (beginning-of-buffer)
  (yank)
  (end-of-buffer)
  (delete-blank-lines)

  (let ((replace (lambda (regex replacement)
                   (goto-char (point-min))
                   (while (re-search-forward regex nil t)
                     (replace-match replacement)))))
    ;; Bullet lists
    ;; (replace-regexp "^\\( *\\)-" "\\,(length \"foo\")•")
    (goto-char (point-min))
    (while (re-search-forward "^\\( *\\)-" nil t)
      (replace-match (concat (replace-regexp-in-string "  " "\t" (match-string 1)) "•")))
    ;; Convert =this= to `this`
    (funcall replace "=\\([^=]+\\)=" "`\\1`")
    ;; Clean up links
    (funcall replace "\\[\\[\\([[:word:]]+:\\([[:word:]]+-\\)?[[:digit:]]+\\)]]" "\\1")
    (funcall replace "\\[\\[\\([[:word:]]+:[[:word:]]+-?[[:digit:]]+\\)]\\[\\([^]]+\\)]]" "_\\2_ (\\1)")
    ;; Subheadings
    (funcall replace "^\\*\\* +\\(.*\\)" "*\\1*")
    (funcall replace "^\\*\\*\\* +\\(.*\\)" "_\\1_")
    ;; Tables
    (funcall replace "\n\n|" "\n\n```\n|")
    (funcall replace "|\n\n" "|\n```\n\n"))

  ;; Make a version in the system paste buffer for pasting into Slack
  (copy-region-as-kill (point-min) (point-max))

  (save-buffer)
  (kill-buffer))

(defun status-to-slack ()
  "Calls `status-to-phone' then copies two days' entries to Slack."
  (interactive)

  (status-to-phone)                     ; kill buffer contains text we want
  (let ((tempfile (make-temp-file "status-to-slack"))
        (json-tempfile (make-temp-file "status-to-slack" nil ".json")))
    (with-temp-file tempfile (yank 1))

    (shell-command (concat "slacker.rb -u jim -c random < " tempfile " > " json-tempfile))
    (shell-command (concat "curl -X POST --silent --data @" json-tempfile " " (getenv "SLACK_WEBHOOK_URL")))
    (delete-file tempfile)
    (delete-file json-tempfile)))
