(lighten-up)

(defun zoom-frame-width-cols ()
  (interactive)				; for testing
  268)                                  ; 268 for really full screen, 260 with dock exposed

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(setq sql-sqlite-program "sqlite3")

;; Start Emacs server
(server-start)
