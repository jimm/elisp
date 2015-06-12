;; A mode for FICO's SRL rules language

(define-generic-mode srl-mode
  '("//" ("/*" . "*/"))                 ;comment-list
  '("if" "then" "else" "apply" "is a" "is an" "as a" "as an" "is some fixed array of" "initially") ;keyword-list
; font-lock-variable-name-face
  '(                                    ;font-lock-list
    ("\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]+\\([ia]s[ \t]+an?\\|is[ \t]some[ \t]fixed[ \t]array[ \t]of\\)[ \t]+" . (1 font-lock-variable-name-face))
    ("[ \t]\\([ia]s[ \t]+an?\\|is[ \t]some[ \t]fixed[ \t]array[ \t]of\\)[ \t]+\\([a-zA-Z_\\.][a-zA-Z0-9_\\.]*\\)[ \t\\.]" . (2 font-lock-type-face))
    )
  '("\\.srl$")                          ;auto-mode-list
  '()                                   ;function-list
  "Major mode for editing FICO's SRL rules files.")

(provide 'srl-mode)
