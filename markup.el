;;; Markup mode (Org, Markdown) extras

(defvar my-markup-bad-mode-msg "Must be in an Org Mode or Markdown buffer")

(defun my-markup-code-word-at-point ()
  "Surrounds the symbol at point with `~` or `\`` to format it as code."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (code-word-char (cond ((eq major-mode 'org-mode) "~")
                              ((eq major-mode 'markdown-mode) "`")
                              (t (error my-markup-bad-mode-msg))))
    (goto-char (cdr bounds))
    (insert code-word-char)
    (goto-char (car bounds))
    (insert code-word-char)
    (goto-char (+ 2 (cdr bounds)))      ; move point after sym+tilde
    (unless (string-equal "\n" (thing-at-point 'char t))
      (forward-char))))

;; Org <-> Markdown link conversion

(defun my-org-mode-text-and-link-at-point ()
  "Returns Org Mode text and link at point as a cons cell."
  (let* ((e (org-element-context))
         (link (org-element-property :raw-link e))
         (text-start (org-element-property :begin e))
         (text-end (org-element-property :end e)))
    (cons (org-link-display-format
           (buffer-substring-no-properties text-start text-end))
          link)))

(defun my-markdown-text-and-link-at-point ()
  "Returns Markdown text and link at point as a cons cell."
  (let ((props (markdown-link-at-pos (point))))
    (cons (nth 2 props)
          (nth 3 props))))

(defun my-markup-text-and-link-at-point ()
  "Copies the link at point to the kill ring and the OS clipboard."
  (interactive)
  (cond ((eq major-mode 'org-mode) (my-org-mode-text-and-link-at-point))
        ((eq major-mode 'markdown-mode) (my-markdown-text-and-link-at-point))
        (t (error my-markup-bad-mode-msg))))

(defun my-markup-convert-link-to-kill-ring ()
  "Converts a link at point from Org Mode to Markdown or vice versa and
stores the text in the kill ring."
  (interactive)
  (let* ((text-and-link (my-markup-text-and-link-at-point))
         (converted
          (cond ((eq major-mode 'org-mode)  ; convert to Markdown
                 (concat "[" (car text-and-link) "](" (cdr text-and-link) ")"))
                ((eq major-mode 'markdown-mode)
                 (concat "[[" (cdr text-and-link) "][" (car text-and-link) "]]"))
                (t (error my-markup-bad-mode-msg)))))
    (message "converted: %s" converted) ;DEBUG
    (with-temp-buffer
      (insert converted)
      (clipboard-kill-ring-save (point-min) (point-max)))))
