(define-generic-mode 'lua-mode
  '("--")				;comment-list
					;keyword-list
  '("if" "then" "else" "elseif" "end" "function" "while" "and" "not" "or"
    "return" "do")
  '(("true" . font-lock-constant-face)	;font-lock-list
    ("false" . font-lock-constant-face))
  '("\\.lua$")				;auto-mode-list
  '((lambda () (auto-fill-mode t)))	;function-list
  "Major mode for editing Lua files.")

(provide 'lua-mode)
