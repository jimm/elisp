(defun zoom-frame-width-cols ()
  "I need to override this because when I'm hooked up to multiple monitors,
`display-pixel-width' returns the wrong value."
  (interactive)
  176)

(line-number-mode 1)                    ; display 'em

(setq dired-use-ls-dired nil
      browse-url-generic-program "open"
      Man-switches "-M /usr/share/man:/usr/local/share/man"
      sql-sqlite-program "sqlite3"
      *my-sql-regex* "^--.*"
      *my-sql-regex-replacement* ""
      *status-file* (concat *my-pim-dir* "orgs/work/candi/status.org")
      emms-source-file-default-directory "~/Documents/Dropbox/Music/music/")

;; Add to the list of directories and files to ignore from rgrep, grep-find,
;; and friends.
(add-to-list 'grep-find-ignored-directories "bundle")
(add-to-list 'grep-find-ignored-files "*[-.]min.js")

;; (when (fboundp #'deft)
;;   (setq deft-directory (concat *my-pim-dir* "orgs/work/candi/")))

;; Markdown
(add-hook 'markdown-mode-hook
          (lambda () (setq markdown-command "multimarkdown")))


;; Set up Deft for searching source code. Unfortunately, this does not do
;; well with a large number of files.
(defun deft-setup (dir extension)
  (interactive "DDirectory: \nsExtension: ")
  (setq deft-directory dir
        deft-recursive t)
  (add-to-list 'deft-extensions extension))

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

  ;; Replace everything above last section.
  (find-file (concat (getenv "dbox") "/Misc/status.txt"))
  (goto-char (point-max))
  (outline-previous-visible-heading 1)
  (delete-region (point-min) (point))
  (yank)

  ;; Swap two days' entries and change headings to "Yesterday" and "Today".
  (outline-previous-visible-heading 1)
  (org-shiftmetaup)

  (org-delete-backward-char 1)
  (org-end-of-line)
  (delete-region (+ 2 (point-min)) (point))
  (insert "Yesterday")

  (outline-next-visible-heading 1)
  (org-open-line 1)
  (forward-char 3)
  (org-kill-line)
  (insert "Today")

  (goto-char (point-min))
  (save-buffer))

;; ================================================================

;;
;; fzf
;;
(when (fboundp 'fzf)
  (setq fzf/executable "/Users/jim.menard/.fzf/bin/fzf"))

;; Start Emacs server
(server-start)

(set-org-file-key [f4] "work/candi/todo.org")
(set-org-file-key [\C-f4] "todo.org")
(global-set-key [f5] #'status)
(global-set-key [\C-f5] (lambda () (interactive) (switch-to-buffer "*SQL*")))
(set-org-file-key [f6] "work/candi/notes.org")
(set-org-file-key [\C-f6] "notes.org")

(require 'my-ruby-mode)                 ; load mode map hook so we can override
(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map "\C-cs" #'run-spec)
            (define-key ruby-mode-map "\C-cp" #'run-spec-at-point)))

(when-fboundp-global-set-key "\C-xo" switch-window)
(when-fboundp-global-set-key [f11]   switch-window)
