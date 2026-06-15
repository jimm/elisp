(when work-orgs-dir
  (set-org-file-key "C-<f4>" (concat "work/" work-orgs-dir "/todo.org"))
  (set-org-file-key "<f6>" (concat "work/" work-orgs-dir "/status.org"))
  (set-org-file-key "C-<f6>" (concat "work/" work-orgs-dir "/notes.org")))
(keymap-global-set "<f5>" #'path-from-git-root-to-clipboard-kill-ring)
