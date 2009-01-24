(define-generic-mode 'chuck-mode
  '("//" ("/*" . "*/"))                 ;comment-list
					;keyword-list
  '("if" "else" "while" "until" "for" "break" "continue"
    "now" "me")
  '(("true\\|false" . font-lock-constant-face))	;font-lock-list
  '("\\.ck$")				;auto-mode-list
  '((lambda () (auto-fill-mode t)))	;function-list
  "Major mode for editing ChucK (http://chuck.cs.princeton.edu) files.")

(provide 'chuck-mode)
