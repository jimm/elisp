(custom-set-variables
 '(python-fill-docstring-style 'pep-257-nn))

(use-package ruff-format)

(let ((pyenv-py "/Users/jimm/.pyenv/shims/python"))
  (when (file-exists-p pyenv-py)
    (setq python-shell-interpreter pyenv-py)))

(add-hook 'python-mode-hook
          (lambda ()
            (turn-on-font-lock)
            (define-key python-mode-map "\C-cx" #'executable-interpret)
            ;; these two are in addition to the \C-< and \C-> bindings
            ;; that already exist in Python mode
            (define-key python-mode-map "\M-[" #'python-indent-shift-left)
            (define-key python-mode-map "\M-]" #'python-indent-shift-right)
	    (ruff-format-on-save)))

;;; Based on pyenv-mode-auto (https://github.com/ssbb/pyenv-mode-auto)
(defun pyenv-mode-auto-hook ()
  "Automatically activates pyenv version if .python-version file
exists, else uses pyenv-defined default, else uses system."
  (when (fboundp #'pyenv-mode-set)
    (let ((local-py-version-file
           (concat (locate-dominating-file default-directory ".python-version") ".python-version")))
      (pyenv-mode-set
       (cond ((file-exists-p local-py-version-file)
              (car (s-lines (s-trim (f-read-text local-py-version-file 'utf-8)))))
             ((file-exists-p "~/.pyenv/version")
              (car (s-lines (s-trim (f-read-text "~/.pyenv/version" 'utf-8)))))
             (t
              "system"))))))
(add-hook 'python-mode-hook 'pyenv-mode-auto-hook)
