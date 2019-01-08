(defvar *light-foreground* "black")
(defvar *light-background* "ghostwhite")
(defvar *light-mode-foreground* "yellow")
(defvar *light-mode-background* "black")

(defvar *dark-foreground* "white")
(defvar *dark-background* "grey10")
(defvar *dark-mode-foreground* "black")
(defvar *dark-mode-background* "orange")

(defvar *org-light-l2-foreground* "black")
(defvar *org-light-block-foreground* "black")
(defvar *org-light-block-border-background* "gray95")
(defvar *org-light-block-background* "ivory")
(defvar *org-light-table-foreground* "black")
(defvar *org-light-table-background* "mint cream")

(defvar *org-dark-l2-foreground* "white")
(defvar *org-dark-block-foreground* "white")
(defvar *org-dark-block-border-background* "gray30")
(defvar *org-dark-block-background* "gray20")
(defvar *org-dark-table-foreground* "mint cream")
(defvar *org-dark-table-background* "gray30")

(defvar *org-presentation-background* "white")
(defvar *current-foreground* nil)
(defvar *current-background* nil)

(defun hello-darkness-my-old-friend ()
  (interactive)
  (ignore-errors
    (set-foreground-color *dark-foreground*)
    (set-background-color *dark-background*)
    (setq *current-foreground* *dark-foreground*
          *current-background* *dark-background*)
    (set-face-attribute 'mode-line nil
                        :foreground *dark-mode-foreground*
                        :background *dark-mode-background*)
    (let ((block-sym (if (>= emacs-major-version 26)
                         'org-block
                       'org-block-background)))
      (when (>= emacs-major-version 24)
        (set-face-attribute 'org-level-2 nil
                            :foreground *org-dark-l2-foreground*)
        (set-face-attribute org-block-sym nil
                            :foreground *org-dark-block-foreground*
                            :background *org-dark-block-background*)
        (set-face-attribute 'org-block-begin-line nil
                            :background *org-dark-block-border-background*)
        (set-face-attribute 'org-block-end-line nil
                            :background *org-dark-block-border-background*)
        (set-face-attribute 'org-table
                            :foreground *org-dark-table-foreground*
                            :background *org-dark-table-background*)))))

(defun lighten-up ()
  (interactive)
  (ignore-errors
    (set-foreground-color *light-foreground*)
    (set-background-color *light-background*)
    (setq *current-foreground* *light-foreground*
          *current-background* *light-background*)
    (set-face-attribute 'mode-line nil
                        :foreground *light-mode-foreground*
                        :background *light-mode-background*)
    (let ((org-block-sym (if (>= emacs-major-version 26)
                         'org-block
                       'org-block-background)))
      (when (>= emacs-major-version 24)
        (set-face-attribute 'org-level-2 nil
                            :foreground *org-light-l2-foreground*)
        (set-face-attribute org-block-sym nil
                            :foreground *org-light-block-foreground*
                            :background *org-light-block-background*)
        (set-face-attribute 'org-block-begin-line nil
                            :background *org-light-block-border-background*)
        (set-face-attribute 'org-block-end-line nil
                            :background *org-light-block-border-background*)
        (set-face-attribute 'org-table
                            :foreground *org-light-table-foreground*
                            :background *org-light-table-background*)))))
