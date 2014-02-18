(setq sql-user "vulcan")
(setq sql-server "10.10.10.17")
(setq sql-database "linksDBProd")

(setq dired-use-ls-dired nil)
(add-hook 'tramp-mode-hook
          (lambda ()
            (add-to-list 'tramp-default-user-alist '("ssh\\|scp" "10\\.10\\.*" "vulcan"))))
(setq sql-sqlite-program "sqlite3")

(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)$" . markdown-mode))
(setq markdown-command "multimarkdown")
