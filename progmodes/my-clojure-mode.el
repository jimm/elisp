(defun reload-clojure-file ()
  (interactive)
  (tell-iterm (concat "(load-file \"" (buffer-file-name) "\")")))

(defun in-ns-to-inferior-lisp ()
  "Send (in-ns 'ns-of-this-buffer) to inferior lisp."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp "(ns \\([a-z][-.a-z0-9_]*\\)" nil t)
      (let ((ns-name (match-string 1)))
        (set-buffer "*inferior-lisp*")
        (goto-char (point-max))
        (insert (concat "(in-ns '" ns-name ")"))
        (comint-send-input)))))

(defun ns-to-inferior-lisp ()
  "Send entire (ns ...) of current buffer to inferior lisp."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp "(ns \\([a-z][-.a-z0-9_]*\\)" nil t)
      (lisp-eval-defun))))

(unless-boundp-setq package-activated-list ())

(setq clojure-mode-hook
      (lambda ()
        (define-key clojure-mode-map "\C-cd" 'debug-comment)
        (define-key clojure-mode-map "\C-ci" 'in-ns-to-inferior-lisp)
        (define-key clojure-mode-map "\C-cn" 'ns-to-inferior-lisp)))
