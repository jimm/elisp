(menu-bar-mode -1)                      ; disable

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

;; Jira
(add-to-list 'org-link-abbrev-alist
             '("jira" . "https://jira.warbyparker.com/browse/"))

;; Markdown
(add-hook 'markdown-mode-hook
          (lambda () (setq markdown-command "multimarkdown")))
