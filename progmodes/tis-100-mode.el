;; See http://www.zachtronics.com/tis-100/

(define-generic-mode tis-100-mode
  '(";" )                               ; comment-list
  '(                                    ; keyword-list
    "NOP"
    "MOV"
    "SWP" "SAV"
    "NEG"
    "ADD" "SUB"
    "JEZ" "JNZ" "JLZ" "JGZ" "JMP" "JRO"
    )
  '(					; font-lock-list
    ("[0-9]+" . 'font-lock-constant-face)
    )
  '("\\.tis100$")                           ; auto-mode-list
  '(
;; (lambda () (auto-fill-mode t))
)	;function-list
  "Major mode for editing TIS-100 files.")

(provide 'tis-100-mode)
