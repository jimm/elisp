(lighten-up)

(defun zoom-frame-width-cols ()
  "I need to override this because when I'm hooked up to multiple monitors,
`display-pixel-width' returns the wrong value."
  (interactive)
  176)

(setq org-agenda-files (list
                        (concat *my-pim-dir* "orgs/work/warby_parker/todo.org")
                        (concat *my-pim-dir* "orgs/todo.org"))
      *status-file* (concat *my-pim-dir*
                            "orgs/work/warby_parker/status_"
                            (format-time-string "%Y")
                            ".org")
      *my-eshell-vcs-maxlen* 32
      emms-source-file-default-directory "~/Documents/Dropbox/Music/music/"
      Buffer-menu-name-width 24)

;; Markdown
(add-hook 'markdown-mode-hook
          (lambda () (setq markdown-command "multimarkdown")))

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
