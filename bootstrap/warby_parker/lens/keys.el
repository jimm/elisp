(global-set-key [f4] #'docker)
(set-org-file-key [\C-f4] "todo.org")
(global-set-key [f5] #'status)
(global-set-key [\C-f5] (lambda () (interactive) (switch-to-buffer "*SQL*")))
(set-org-file-key [f6] #'pyfmt)
(set-org-file-key [\C-f6] "notes.org")
(global-set-key "\C-cc" #'same-file-other-dir)

(when-fboundp-global-set-key "\C-xo" switch-window)
(when-fboundp-global-set-key [f11]   switch-window)

(defun htest-current-buffer (arg)
  (interactive "p")
  (htest arg (buffer-file-name)))

(let ((m (define-prefix-command 'my-python-proj-map))
      (h (define-prefix-command 'my-python-proj-helios-map)))
  (define-key h "v" #'htest-current-buffer)
  (define-key m "v" #'htest-current-buffer)
  (define-key m "h" 'my-python-proj-helios-map))

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "C-c ,") 'my-python-proj-map)))
