(defun zoom-frame-width-cols ()
  (interactive)				; for testing
  (round (/ (float (display-pixel-width))
            (+ (float (frame-char-width)) 0.175))))

(setq sql-user "root")
(setq sql-server "10.10.10.43")
(setq sql-database "linksDBProd")

(setq dired-use-ls-dired nil)

(setq sql-sqlite-program "sqlite3")

(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)$" . markdown-mode))
(setq markdown-command "multimarkdown")
