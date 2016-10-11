;; See http://www.zachtronics.com/shenzhen-io/

;; (defface keymaster-note-face
;;   '((t (:foreground "ForestGreen")))
;;   "keymaster mode option face")

(define-generic-mode shenzhen-io-mode
  '("#" )                               ; comment-list
  '(                                    ; keyword-list
    "nop" "mov" "jmp" "slp" "slx" "gen"
    "add" "sub" "mul" "not" "dgt" "dst"
    "tlt" "teq" "tgt" "tcp"
    )
  '(					; font-lock-list
    ("," . 'font-lock-warning-face)
    ("[px][0-9]+" . 'font-lock-reference-face)
    ("[0-9]+" . 'font-lock-constant-face)
    )
  '("\\.szio$")                         ; auto-mode-list
  '(                                    ; function-list
    (lambda () (setq comment-column 0))
    )
  "Major mode for editing Shenzhen I/O files.")

(provide 'shenzhen-io-mode)
