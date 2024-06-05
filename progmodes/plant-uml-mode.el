;;; A generic mode for sequencediagram.org diagrams
;;; See https://sequencediagram.org/instructions.html

(define-generic-mode plant-uml-mode
  '("' ")                               ; comment-list
  '(                                    ; keyword-list
    "title"

    ;; participants
    "participant" "database" "actor" "boundary" "control"
    "database" "entity" "materialdesignicons" "fontawesome6solid"
    "fontawesome6regular" "fontawesome6brands" "fontawesome"
    "image"
    "bottomparticipants"

    "note"

    "create" "destroy" "destroyafter" "destroysilent"

    "activate" "deactivate" "deactivateafter" "autoactivation"

    "space"

    ;; fragments
    "alt" "opt" "loop" "par" "break" "critical" "ref" "seq"
    "strict" "neg" "ignore" "consider" "assert" "region"
    "group" "expandable"

    ;; participant groups
    "participantgroup"

    "frame"

    ;; fragments, participant groups
    "end"
    )
  '(					; font-lock-list
    (
     "\\([[:alnum:]]+\\|\"[[:alnum:]][[:space:][:alnum:]]\"*\\|\\[\\|\\]\\)[-<>]+\\([[:alnum:]]+\\|\"[[:alnum:]][[:space:][:alnum:]]\"*\\|\\[\\|\\]\\):"
     (1 'font-lock-variable-name-face)
     (2 'font-lock-variable-name-face))
    ("#[A-Za-z0-9]+" . 'font-lock-string-face))
  '("\\.seqd$")                         ; auto-mode-list
  '(                                    ; function-list
    ;; (lambda () (auto-fill-mode t))
    )
  "Major mode for editing sequencediagram.org files.")

(provide 'plant-uml-mode)
