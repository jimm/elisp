(defvar work-orgs-dir nil
  "Name of $pim/orgs/work subdir where I keep work-related Org mode files.")

(when window-system
  (let ((basic-frame-alist '((tool-bar-mode . nil)
                             (font . "Menlo 13"))))
    (set-face-attribute 'default t :font "Menlo 13")
    (setq
     initial-frame-alist (append basic-frame-alist '((fullscreen . fullheight)))
     default-frame-alist basic-frame-alist)))
