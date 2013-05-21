;; (defface keymaster-note-face
;;   '((t (:foreground "ForestGreen")))
;;   "keymaster mode option face")

(define-generic-mode 'keymaster-mode
  '("//" "/\\*.*\\*/")                ;comment-list
  '(                                  ;keyword-list
    "input" "output"
    "trigger"
    "message"
    "messageKey" "messagekey"
    "chain"
    "song"
    "notes"
    "end"
    "patch"
    "startBytes" "stopBytes" "startbytes" "stopbytes"
    "connection" "conn" "c"
    "transpose" "xpose" "x"
    "pc" "programchange" "progchg" "programChange" "progChg"
    "zone"
    "filter")
  '(					;font-lock-list
    ("[a-gA-G][#sb]?-?[0-9]+" . 'font-lock-constant-face)
    )
  '("\\.km$")                           ;auto-mode-list
  '(
;; (lambda () (auto-fill-mode t))
)	;function-list
  "Major mode for editing KeyMaster files.")

(provide 'keymaster-mode)
