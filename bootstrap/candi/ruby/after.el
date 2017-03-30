(lighten-up)

(defun zoom-frame-width-cols ()
  "I need to override this because when I'm hooked up to multiple monitors,
`display-pixel-width' returns the wrong value."
  (interactive)
  176)

;; Fix for Max OS X 10.11.1 El Capitan problem
(setq visible-bell nil)
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit exit-minibuffer
                        mwheel-scroll down up next-line previous-line
                        backward-char forward-char))
          (invert-face 'mode-line)
          (run-with-timer 0.1 nil 'invert-face 'mode-line))))

(setq *status-file* (concat *my-pim-dir* "orgs/work/candi/status.org")
      emms-source-file-default-directory "~/Documents/Dropbox/Music/music/")

;; Add to the list of directories and files to ignore from rgrep, grep-find,
;; and friends.
(add-to-list 'grep-find-ignored-directories "bundle")
(add-to-list 'grep-find-ignored-files "*[-.]min.js")

(require 'inf-ruby)
(add-to-list 'inf-ruby-implementations
             '("heroku-prod" . "candi-heroku candiprod2"))
(add-to-list 'inf-ruby-implementations
             '("heroku-staging" . "candi-heroku candistaging"))
(add-to-list 'inf-ruby-implementations
             '("heroku-jim" . "candi-heroku jim-qa"))
(add-to-list 'inf-ruby-implementations
             '("heroku-sean" . "candi-heroku sean-qa"))
(add-to-list 'inf-ruby-implementations
             '("heroku-charlie" . "candi-heroku charlie-qa"))
(add-to-list 'inf-ruby-implementations
             '("rails-console" . "candi-console"))

;; (when (fboundp #'deft)
;;   (setq deft-directory (concat *my-pim-dir* "orgs/work/candi/")))

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

;; This should be a YASnippet, but It's work specific, which means I should
;; isolate it somewhere first.
(defun dcdsi ()
  (interactive)
  (insert-string "Dotcom::Fulfillment::ShipmentInfo.all_from_dcd_order_number('')")
  (backward-char 2))

;; ================================================================
;; Status
;; ================================================================

(defun status-to-phone ()
  "Moves most recent two days' entries from *status-file* into a
Dropbox file that I can read from my phone. Useful for standup
meetings."
  (interactive)

  (save-excursion
    ;; Grab last two days' entries.
    (find-file *status-file*)
    (goto-char (point-min))
    (org-forward-heading-same-level 2)
    (copy-region-as-kill (point-min) (point))
    (goto-char (point-min))

    ;; Replace everything above last section.
    (find-file (concat (getenv "dbox") "/Miscellaneous/status.txt"))
    (goto-char (point-min))
    (yank)
    (delete-region (point) (point-max))

    ;; Swap two days' entries and change headings to "Yesterday" and
    ;; "Today".
    (goto-char (point-min))
    (org-forward-heading-same-level 2)
    (org-move-subtree-up)

    (org-delete-backward-char 1)
    (org-end-of-line)
    (delete-region (+ 2 (point-min)) (point))
    (insert "Yesterday")

    (org-forward-heading-same-level 1)
    (org-open-line 1)
    (forward-char 3)
    (org-kill-line)
    (insert "Today")

    (goto-char (point-max))
    (delete-blank-lines)
    (insert "\n* Local Variables\n\nThese are for Emacs.\n\n# Local Variables:\n#   mode: org\n# End:\n")

    (goto-char (point-min))
    (save-buffer)
    (bury-buffer)))

;;; ================================================================

;;
;; Org Present Mode
;;
;; https://github.com/rlister/org-present
(add-hook 'org-present-mode-hook
          (lambda ()
            (set-background-color "White")))
(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (set-background-color "GhostWhite")))


;;;
;;; fzf
;;;
(when (fboundp 'fzf)
  (setq fzf/executable "/Users/jim.menard/.fzf/bin/fzf"))

;; Start Emacs server
(server-start)
