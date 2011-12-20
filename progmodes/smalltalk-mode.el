(define-generic-mode smalltalk-mode
  '((?". ?"))                           ;comment-list
  ;keyword-list
  '("self")
  '(("'[^']+'" . 'font-lock-string-face)) ;font-lock-list
  '("\.st$")				;auto-mode-list
  '((lambda () (auto-fill-mode nil)))	;function-list
  "Major mode for editing Smalltalk files.")

(provide 'smalltalk-mode)
