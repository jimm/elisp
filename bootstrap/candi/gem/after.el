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

(add-to-list 'org-capture-templates
             '("k" "Ticket w/checklist" entry (file+headline "work/candi/todo.org" "Miscellaneous")
               "* TODO %?\n   [[jira:CAN-??]]\n   - [ ] Development\n   - [ ] PR +1\n   - [ ] QA pass\n   - [ ] Merge, delete branch"
               :prepend t
               :empty-lines 1)
             t)

;; Add to the list of directories and files to ignore from rgrep, grep-find,
;; and friends.
(add-to-list 'grep-find-ignored-directories "bundle")
(add-to-list 'grep-find-ignored-files "*[-.]min.js")

(require 'inf-ruby)

(defun -heroku-config-impl-entries ()
  "Reads $candi/.git/config and returns a list of cons cells
suitable for `inf-ruby-implementations`."
  (find-file-read-only (concat (getenv "candi") "/.git/config"))
  (let ((envs ()))
    (while (search-forward "[remote \"" nil t)
      (let ((start (point)))
        (search-forward "\"")
        (let ((name (buffer-substring-no-properties start (- (point) 1))))
          (unless (member name '("heroku" "origin"))
            (setq envs (cons (cons name
                                   (concat "candi-heroku " name))
                             envs))
            (setq envs (cons (cons (concat name "-size-m")
                                   (concat "candi-heroku " name " --size=performance-m"))
                             envs))))))
    (kill-buffer)
    envs))

;; Add Heroku configs plus a few more to inf-ruby-implementations
(mapc (lambda (impl) (push impl inf-ruby-implementations))
      (append (-heroku-config-impl-entries)
              (list '("rails-console" . "candi-console")
                    '("spring-rails-console"  . "candi-console --spring"))))

;; Markdown
(add-hook 'markdown-mode-hook
          (lambda () (setq markdown-command "multimarkdown")))

;; Org mode links
(add-to-list 'org-link-abbrev-alist '("jira" . "https://chloeandisabel.atlassian.net/browse/"))
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
Dropbox file that I can read from my phone. Also copies it into
the system paste buffer, ready for Slack. Useful for daily
standup meetings."
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

;;; ================================================================

;;
;; Org Present Mode
;;
;; Note that there's a bug in the (old) version of Org mode that Org Present
;; depends on that breaks table formatting.
;;
;; https://github.com/rlister/org-present
(defvar *org-present-orig-background* nil)
(add-hook 'org-present-mode-hook
          (lambda ()
            (set-background-color *org-presentation-background*)))
(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (set-background-color *current-background*)))

;; Start Emacs server
(server-start)
