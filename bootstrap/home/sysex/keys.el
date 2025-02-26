(when work-orgs-dir
  (global-set-key [\C-f4]
                  (lambda ()
                    (interactive)
                    (find-file (concat *my-pim-dir* (concat "orgs/work/" work-orgs-dir "/todo.org")))))
  (global-set-key [\C-f6]
                  (lambda ()
                    (interactive)
                    (find-file (concat *my-pim-dir* (concat "orgs/work/" work-orgs-dir "/notes.org"))))))
(global-set-key [f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/todo.org"))))
