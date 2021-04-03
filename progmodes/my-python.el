(defun pyfmt-default-format-buffer ()
  "Format the current Python buffer."
  (if (fboundp #'py-isort-buffer)
      (py-isort-buffer)
    (save-buffer)
    (call-process "isort" nil nil nil (shell-quote-argument (buffer-file-name)))
    (revert-buffer t t))
  (if (fboundp #'python-black-buffer)
      (python-black-buffer)
    (save-buffer)
    (shell-command (concat "black --quiet " (shell-quote-argument (buffer-file-name))))
    (revert-buffer t t)))

(defvar *prevent-python-formatting* nil
  "This is a buffer-local variable that prevents `pyfmt' from
  running when it is non-`nil'.")

(defvar *pyfmt-format-buffer-func* #'pyfmt-default-format-buffer
  "This is a buffer-local variable that is a function that will
  be called with no arguments.")

(defun pyfmt (&optional arg)
  "Format the current Python buffer by calling `*pyfmt-format-buffer-func*'.

If ARG is > 1, force formatting even if
*prevent-python-formatting* is `nil'. ARG is 1 by default.

If ARG is < 0, skip formatting even if
*prevent-python-formatting* is non-`nil'.

Else, do nothing if the current buffer's major mode is not
`python-mode' or if the buffer-local variable
`*prevent-python-formatting*' is non-`nil'."
  (interactive "p")
  (setq arg (or arg 1))
  (when (and (eq major-mode #'python-mode)
             (> arg 0)
             (or (> arg 1)
                 (not *prevent-python-formatting*)))
    (funcall *pyfmt-format-buffer-func*)))

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(custom-set-variables
 '(python-fill-docstring-style 'pep-257-nn))
(add-hook 'python-mode-hook
          (lambda ()
            (turn-on-font-lock)
            (define-key python-mode-map "\C-cx" #'executable-interpret)
            ;; these two are in addition to the \C-< and \C-> bindings
            ;; that already exist in Python mode
            (define-key python-mode-map "\M-[" #'python-indent-shift-left)
            (define-key python-mode-map "\M-]" #'python-indent-shift-right)
            (add-hook 'after-save-hook #'pyfmt nil t)))

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
