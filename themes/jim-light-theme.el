(deftheme jim-light "My light theme.")

(let ((org-mode-block-sym (if (>= emacs-major-version 26)
                              'org-block
                            'org-block-background)))
  (custom-theme-set-faces
   'jim-light
   `(default ((t (:foreground "black" :background "ghostwhite"))))
   `(mode-line ((t (:foreground "yellow" :background "black"))))
   `(eshell-prompt ((t (:foreground "blue" :bold t))))

   `(org-level-2 ((t (:foreground "black" :bold t))))
   `(,org-mode-block-sym ((t (:foreground "black"
                     :background "ivory"))))
   `(org-block-begin-line ((t (:background "gray95"))))
   `(org-block-end-line ((t (:background "gray95"))))
   `(org-table ((t (:foreground "black"
                    :background "mint cream"))))
   `(org-code ((t (:foreground "dark cyan"
                   :background "ghostwhite"))))


   `(markdown-header-face-2 ((t (:foreground "black" :bold t))))
   `(markdown-header-face-3 ((t (:foreground "purple"))))
   `(markdown-header-face-4 ((t (:foreground "red"))))
   `(markdown-header-face-5 ((t (:foreground "green"))))

   `(cypher-variable-face ((t (:foreground "black"))))
   ))

(provide-theme 'jim-light)
