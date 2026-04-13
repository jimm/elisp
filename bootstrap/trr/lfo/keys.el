(when work-orgs-dir
  (keymap-global-set "C-<f4>"
                  (lambda ()
                    (interactive)
                    (find-file (concat *my-pim-dir* (concat "orgs/work/" work-orgs-dir "/todo.org")))))
  (keymap-global-set "C-<f6>"
                  (lambda ()
                    (interactive)
                    (find-file (concat *my-pim-dir* (concat "orgs/work/" work-orgs-dir "/notes.org"))))))
(keymap-global-set "<f4>"
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/todo.org"))))2
