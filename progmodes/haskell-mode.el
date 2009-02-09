(define-generic-mode 'haskell-mode
  '("--")				;comment-list
  ;keyword-list
  '("case" "data" "deriving" "do" "import" "let" "module" "return" "<-" "->")
  '()					;font-lock-list
  '("\.hs$")				;auto-mode-list
  '((lambda () (auto-fill-mode t)))	;function-list
  "Major mode for editing Haskell files.")

(provide 'haskell-mode)
