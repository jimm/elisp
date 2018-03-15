(lighten-up)

(defun zoom-frame-width-cols ()
  "I need to override this because when I'm hooked up to multiple monitors,
`display-pixel-width' returns the wrong value."
  (interactive)
  176)

(setq org-agenda-files (list
                        (concat *my-pim-dir* "orgs/work/candi/todo.org")
                        (concat *my-pim-dir* "orgs/todo.org"))
      *status-file* (concat *my-pim-dir* "orgs/work/candi/status.org")
      *my-eshell-vcs-maxlen* 32
      emms-source-file-default-directory "~/Documents/Dropbox/Music/music/"
      Buffer-menu-name-width 24)

;; Add to the list of directories and files to ignore from rgrep, grep-find,
;; and friends.
(add-to-list 'grep-find-ignored-directories "bundle")
(add-to-list 'grep-find-ignored-files "*[-.]min.js")

(require 'inf-ruby)
(mapc (lambda (impl) (push impl inf-ruby-implementations))
      (list '("heroku-prod"    . "candi-heroku candiprod2")
            '("heroku-staging" . "candi-heroku candistaging")
            '("heroku-uat"     . "candi-heroku candiuat")
            '("heroku-jim"     . "candi-heroku jim-qa")
            '("heroku-kajal"   . "candi-heroku kajal-qa")
            '("rails-console"  . "candi-console")
            '("spring-rails-console"  . "candi-console --spring")))

;; Markdown
(add-hook 'markdown-mode-hook
          (lambda () (setq markdown-command "multimarkdown")))

(add-to-list 'org-link-abbrev-alist '("jira" . "https://chloeandisabel.atlassian.net/browse/CAN-"))
(mapc (lambda (pair)
        (add-to-list 'org-link-abbrev-alist
         (cons (car pair)
               (concat "https://github.com/chloeandisabel/Candi/" (cdr pair) "/"))))
      '(("issue" . "issues")
        ("order" . "orders")
        ("pr"    . "pull")
        ("pull"  . "pull")))

(defun irbrc-to-other-buffer ()
  (interactive)
  (save-excursion
    (find-file "~/.irbrc")
    (mark-whole-buffer)
    (kill-ring-save (point) (mark))

    (other-window 1)
    (end-of-buffer)
    (yank)
    (comint-send-input)

    (other-window -1)
    (kill-buffer nil)))

(defun same-file-other-dir ()
  "When run from a buffer visiting a file or directory in $candi or $candi2,
opens the same thing from the other directory using
`find-file-other-window'."
  (interactive)
  (let* ((path (or (buffer-file-name) default-directory)))
    (string-match "\\(/candi2?/\\)" path)
    (find-file-other-window
     (if (string= "/candi2/" (match-string 0 path))
         (replace-regexp-in-string "/candi2/" "/candi/" path)
       (replace-regexp-in-string "/candi/" "/candi2/" path)))))

;; ================================================================
;; Status
;; ================================================================

(defun status-to-phone ()
  "Moves most recent two days' entries from *status-file* into a
Dropbox file that I can read from my phone. Useful for standup
meetings."
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

  ;; Swap two days' entries and change headings to "Yesterday" and
  ;; "Today".
  (goto-char (point-min))
  (org-move-subtree-down)
  (org-open-line 1)

  (goto-char (point-min))
  (org-delete-char 1)
  (forward-char 2)
  (org-kill-line)
  (insert "Yesterday")
  (delete-char 1)

  (org-forward-heading-same-level 1)
  (forward-char 2)
  (org-kill-line)
  (insert "Today")
  (delete-char 1)

  (goto-char (point-max))
  (delete-blank-lines)
  (insert "\n* Local Variables\n# Local Variables:\n#   mode: org\n# End:\n")

  ;; Clean up links
  (goto-char (point-min))
  (while (re-search-forward "\\[\\[\\([:word:]+:[:digit:]+\\)]]" nil t)
    (replace-match "\\1"))
  (goto-char (point-min))
  (while (re-search-forward "\\[\\[[[:word:]]+:[[:digit:]]+]\\[\\([^]]+\\)]]" nil t)
    (replace-match "\\1"))

  (save-buffer)
  (kill-buffer))

(defun status-to-slack ()
  "Calls `status-to-phone' then copies two days' entries to Slack."
  (interactive)

  (status-to-phone)

  ;; Open file, copy entries
  (find-file (concat (getenv "dbox") "/Miscellaneous/status.txt"))
  (goto-char (point-min))
  (org-forward-heading-same-level 2)
  (copy-region-as-kill (point-min) (point))
  (kill-buffer)

  (let ((tempfile (make-temp-file "status-to-slack")))
    (find-file tempfile)
    (insert "```\n")
    (yank 1)
    (insert "```\n")
    (save-buffer)
    (kill-buffer)

    (shell-command (concat "slacker.rb -u jim -c random < " tempfile " > /tmp/slack_post.json"))
    (shell-command (concat "curl -X POST --silent --data @/tmp/slack_post.json " (getenv "SLACK_WEBHOOK_URL")))
    (delete-file tempfile)
    (delete-file "/tmp/slack_post.json")))

;;; ================================================================

;;
;; Org Present Mode
;;
;; https://github.com/rlister/org-present
(defvar *org-present-orig-background* nil)
(add-hook 'org-present-mode-hook
          (lambda ()
            (set-background-color *org-presentation-background*)))
(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (set-background-color *current-background*)))


;;;
;;; fzf
;;;
(when (fboundp 'fzf)
  (setq fzf/executable "/Users/jim.menard/.fzf/bin/fzf"))

;; Start Emacs server
(server-start)
