(add-to-list 'load-path "/Users/jimm/src/emms/lisp/")
(require 'emms-setup)
(emms-standard)
(emms-default-players)

;; TODO Specific to my work setup. These rebindings should be moved into
;; bootstrap/nrelate/widget/after.el.
(when (fboundp 'vulcan-password-to-iterm)
  (global-set-key [f5] 'vulcan-password-to-iterm)
  (global-set-key [\C-f5] 'mindspark-password-to-clipboard))

(global-set-key [f7] 'emms-previous)
(global-set-key [f8] 'emms-pause)       ; toggles between pause and resume
(global-set-key [f9] 'emms-next)
