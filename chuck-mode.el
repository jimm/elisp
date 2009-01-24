(define-generic-mode 'chuck-mode
  '("//" ("/*" . "*/"))                 ;comment-list
					;keyword-list
  '(; primitive types
    "int" "float" "time" "dur" "void"
    ; (unimplemented primitive types)
    "same"
    ; control structures
    "if" "else" "while" "until" "for" "repeat" "break" "continue" "return"
    ; (unimplemented control structures)
    "switch"
    ; class keywords
    "class" "extends" "public" "static" "pure" "this"
    ; (unimplemented class keywords)
    "super" "interface" "implements" "protected" "private"
    ; other
    "function" "fun" "spork" "const" "new"
    ; special values (only some; others are constants below)
    "now"
    ; special: default durations
    "samp" "ms" "second" "minute" "hour" "day" "week"
    ; special: global ugens
    "dac" "adc" "blackhole"
    )
                                        ;font-lock-list
  '(("true\\|false\\|maybe\\|null\\|NULL\\|me\\|pi" . font-lock-constant-face))
  '("\\.ck$")				;auto-mode-list
  '((lambda () (auto-fill-mode t)))	;function-list
  "Major mode for editing ChucK (http://chuck.cs.princeton.edu) files.")

(provide 'chuck-mode)
