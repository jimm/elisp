(defface rdoc-header-1
  '((t ( ; :background "black" :foreground "white"
		  :weight bold :height 1.75)))
  "rdoc mode header level 1")

(defface rdoc-header-2
  '((t (; :foreground "blue"
        :weight bold :height 1.33)))
  "rdoc mode header level 2")

(defface rdoc-header-3
  '((t (; :foreground "black"
        :weight bold)))
  "rdoc mode header level 3")

(defface rdoc-header-4
  '((t (:foreground "darkblue"
        :weight bold)))
  "rdoc mode header level 4")

(defface rdoc-code
  '((t (:foreground "gray50")))
  "rdoc mode for code")

(defface rdoc-bullet-list-bullet
  '((t (:foreground "blue" weight: bold)))
  "rdoc mode header level 4")

(define-generic-mode 'rdoc-mode
  '()					;comment-list
  '()					;keyword-list
  '(					;font-lock-list
    ("^=[^=].*" . 'rdoc-header-1)
    ("^==[^=].*" . 'rdoc-header-2)
    ("^===[^=].*" . 'rdoc-header-3)
    ("^====[^=].*" . 'rdoc-header-4)
    ("^[ \t]*[\\*#]\\{1,9\\} " . 'rdoc-bullet-list-bullet)
    ("\\(?:[^a-zA-Z0-9]\\)?\\*[^*]+\\*\\(?:[^a-zA-Z0-9]\\)" . 'bold)
    ("\\(?:[^a-zA-Z0-9]\\)?_[^_]+_\\(?:[^a-zA-Z0-9]\\)" . 'italic)
    ("\\(?:[^a-zA-Z0-9]\\)?\\+[^+]+\\+\\(?:[^a-zA-Z0-9]\\)" . 'rdoc-code)
    )
  '("README_FOR_APP" "\\.rdoc$")	;auto-mode-list
  '((lambda () (auto-fill-mode t)))	;function-list
  "Major mode for editing RDOC files.")

(provide 'rdoc-mode)
