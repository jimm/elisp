(global-set-key [\C-f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/kaplan.org"))))
(global-set-key [f4]
  (lambda ()
    (interactive)
    (find-file (concat *my-pim-dir* "orgs/todo.org"))))
