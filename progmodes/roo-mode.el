(defface roo-option-face
  '((t (:foreground "blue")))
  "roo mode option face")

(define-generic-mode 'roo-mode        ;quote still allowed for backwd compat
  '("//" "/\\*.*\\*/")                ;comment-list
  '(                                  ;keyword-list
    "project"
    "persistence" "setup"
    "entity"
    "field" "boolean" "date" "email" "enum" "jms" "number" "reference" "set" "string"
    "controller" "class"
    "perform" "assembly" "clean" "command" "eclipse" "package" "tests"
    "logging")
  '(					;font-lock-list
    ("--[-_a-zA-Z0-9]+" . 'roo-option-face)
    ;; ("^=[^=].*" . 'font-loc)
    ;; ("^==[^=].*" . 'roo-header-2)
    ;; ("^===[^=].*" . 'roo-header-3)
    ;; ("^====[^=].*" . 'roo-header-4)
    ;; ("^[ \t]*[\\*#]\\{1,9\\} " . 'roo-bullet-list-bullet)
    ;; ("\\(?:[^a-zA-Z0-9]\\)?\\*[^*]+\\*\\(?:[^a-zA-Z0-9]\\)" . 'bold)
    ;; ("\\(?:[^a-zA-Z0-9]\\)?_[^_]+_\\(?:[^a-zA-Z0-9]\\)" . 'italic)
    ;; ("\\(?:[^a-zA-Z0-9]\\)?\\+[^+]+\\+\\(?:[^a-zA-Z0-9]\\)" . 'roo-code)
    )
  '("\\.roo$")	;auto-mode-list
  '(
;; (lambda () (auto-fill-mode t))
)	;function-list
  "Major mode for editing ROO files.")

(provide 'roo-mode)
