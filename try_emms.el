(add-to-list 'load-path "/Users/jimm/src/emms/lisp/")
(require 'emms-setup)
(emms-standard)

(global-set-key [f7] 'emms-previous)
(global-set-key [f8] 'emms-pause)       ; toggles between pause and resume
(global-set-key [f9] 'emms-next)
