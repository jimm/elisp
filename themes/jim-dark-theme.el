(deftheme jim-dark "My dark theme.")

(let ((org-mode-block-sym (if (>= emacs-major-version 26)
                              'org-block
                            'org-block-background)))
  (custom-theme-set-faces
   'jim-dark
   `(default ((t (:foreground "white" :background "grey10"))))
   `(mode-line ((t (:foreground "black" :background "orange"))))
   `(eshell-prompt ((t (:foreground "LightBlue" :bold t))))

   `(org-level-2 ((t (:foreground "yellow" :bold t))))
   `(,org-mode-block-sym ((t (:foreground "white"
                     :background "gray20"))))
   `(org-block-begin-line ((t (:background "gray30"))))
   `(org-block-end-line ((t (:background "gray30"))))
   `(org-table ((t (:foreground "mint cream"
                    :background "gray 30"))))
   `(org-code ((t (:foreground "cyan"
                   :background "grey10"))))


   `(markdown-header-face-2 ((t (:foreground "white" :bold t))))
   `(markdown-header-face-3 ((t (:foreground "purple"))))
   `(markdown-header-face-4 ((t (:foreground "red"))))
   `(markdown-header-face-5 ((t (:foreground "green"))))

   `(cypher-variable-face ((t (:foreground "white"))))
   ))

(provide-theme 'jim-dark)
