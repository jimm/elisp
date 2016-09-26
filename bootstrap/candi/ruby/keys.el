(set-org-file-key [f4] "work/candi/todo.org")
(set-org-file-key [\C-f4] "todo.org")
(global-set-key [f5] #'status)
(global-set-key [\C-f5] (lambda () (interactive) (switch-to-buffer "*SQL*")))
(set-org-file-key [f6] "work/candi/notes.org")
(set-org-file-key [\C-f6] "notes.org")

(when-fboundp-global-set-key "\C-xo" switch-window)
(when-fboundp-global-set-key [f11]   switch-window)
