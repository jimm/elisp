(defvar *my-colors*
  '((light . ((foreground . "black")
              (background . "ghostwhite")
              (mode . ((foreground . "yellow")
                       (background . "black")))
              (org . ((l2-foreground . "black")
                      (block-foreground . "black")
                      (block-border-background . "gray95")
                      (block-background . "ivory")
                      (table-foreground . "black")
                      (table-background . "mint cream")))))
    (dark . ((foreground . "white")
             (background . "grey10")
             (mode . ((foreground . "black")
                      (background . "orange")))
             (org . ((l2-foreground . "white")
                     (block-foreground . "white")
                     (block-border-background . "gray30")
                     (block-background . "gray20")
                     (table-foreground . "mint cream")
                     (table-background . "gray30")))))
    (dim . ((foreground . "white")
             (background . "dark slate gray")
             (mode . ((foreground . "black")
                      (background . "orange")))
             (org . ((l2-foreground . "white")
                     (block-foreground . "white")
                     (block-border-background . "gray30")
                     (block-background . "gray20")
                     (table-foreground . "mint cream")
                     (table-background . "gray30")))))))

(defvar *org-presentation-background* "white")
(defvar *current-foreground* nil)
(defvar *current-background* nil)

(defun set-colors (colors-alist)
  (ignore-errors
    (let ((fg (cdr (assoc 'foreground colors-alist)))
          (bg (cdr (assoc 'background colors-alist))))
      (set-foreground-color fg)
      (set-background-color bg)
      (setq *current-foreground* fg
            *current-background* bg))
    (let ((mode-alist (cdr (assoc 'mode colors-alist))))
      (set-face-attribute 'mode-line nil
                          :foreground (cdr (assoc 'foreground mode-alist))
                          :background (cdr (assoc 'background mode-alist))))
    (let ((org-alist (cdr (assoc 'org colors-alist))))
      (let ((block-sym (if (>= emacs-major-version 26)
                           'org-block
                         'org-block-background)))
        (when (>= emacs-major-version 24)
          (set-face-attribute 'org-level-2 nil
                              :foreground (cdr (assoc 'l2-foreground org-alist)))
          (set-face-attribute block-sym nil
                              :foreground (cdr (assoc 'block-foreground org-alist))
                              :background (cdr (assoc 'block-background org-alist)))
          (set-face-attribute 'org-block-begin-line nil
                              :background (cdr (assoc 'block-border-background org-alist)))
          (set-face-attribute 'org-block-end-line nil
                              :background (cdr (assoc 'block-border-background org-alist)))
          (set-face-attribute 'org-table nil
                              :foreground (cdr (assoc 'table-foreground org-alist))
                              :background (cdr (assoc 'table-background org-alist))))))))

(defun set-my-colors (my-colors-sym)
  (interactive "SColor theme name: ")
  (set-colors (assoc my-colors-sym *my-colors*)))
