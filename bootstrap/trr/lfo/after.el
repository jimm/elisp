(load-theme 'jim-light)

(setq user-email-address "jim.menard@therealreal.com"
      dired-use-ls-dired nil
      browse-url-generic-program "open"
      Man-switches "-M /usr/share/man:/opt/homebrew/share/man"
      sql-sqlite-program "sqlite3"
      rubocopfmt-rubocop-command "rubocop-daemon-wrapper" ; overridden in $trr/.dir-locals.el
      my-shell #'shell
      my-alternate-shell #'eshell)

;; ;;; experimentation

;; (defvar trr-link-regexp
;;   "Regular expression matching a URL that should go to my work browser."
;;   (rx (or "therealreal"
;;           "TheRealReal"
;;           "app.notion.com"
;;           "bugsnag"
;;           "datadog")))

;; (defun trr-link-p (link)
;;   (and (org-url-p link)
;;        (string-match-p trr-link-regexp link)))

;; (defun my-org-maybe-open-url (&optional arg)
;;   "A hook for opening URLs in the appropriate browser: Chrome for work, Firefox for other.

;; See `org-open-at-point-functions'."
;;   (let ((link (org--link-at-point)))
;;     (message "Org link at point: \"%s\"" (org--link-at-point))
;;     (cond ((trr-link-p link)
;;            (start-process (concat "open " link) nil "open" "-a" "Chrome" link))
;;           ((org-url-p link)
;;            (start-process (concat "open " link) nil "open" "-a" "Firefox" link)))))

;; (add-hook 'org-open-at-point-functions #'my-org-maybe-open-url)





;; (defun trr-url-p (url)
;;   "Returns non-nil if `url` is a work URL and needs to be opened with the
;; default browser."
;;   (string-match-p "[Rr]eal[rR]eal\\|trr-" url))

;; (defun open-with-firefox (url &rest _args)
;;   "Opens url with Firefox."
;;   (let ((encoded-url (browse-url-encode-url url)))
;;     (start-process (concat "open " encoded-url) nil "open" "-a" "Firefox" encoded-url)))

;; (setq
;;       browse-url-handlers '((trr-url-p . browse-url))
;;       browse-url-default-handlers (cons '("^http" . open-with-firefox) browse-url-default-handlers))

;; ; DEBUG default values
;; (setq browse-url-handlers nil
;;       browse-url-default-handlers '(("\\`mailto:" . browse-url--mailto)
;;                                     ("\\`man:" . browse-url--man)
;;                                     ("\\`irc6?s?://" . browse-url--irc)
;;                                     (browse-url--non-html-file-url-p . browse-url-emacs)))

(defun trr-path (&optional arg)
  "Copies relative path of file visited by current buffer from parent dir
~/src/trr to the kill ring and GUI clipboard. Returns relative path.

With an ARG, append the line number at point."
  (interactive "p")
  (let ((absolute-path (or (buffer-file-name) default-directory)))
    (when absolute-path
      (let* ((trr-root-dir "/Users/jim.menard/src/trr/")
             (relative-path (substring absolute-path (length trr-root-dir))))
        (when relative-path
          (-path-to-clipboard-kill-ring relative-path (> (or arg 1) 1)))))))
