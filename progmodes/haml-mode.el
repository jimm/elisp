;;; haml-mode.el -- Major mode for editing Haml files
;;; Written by Nathan Weizenbaum

;;; Because Haml's indentation schema is similar
;;; to that of YAML and Python, many indentation-related
;;; functions are similar to those in yaml-mode and python-mode.

;;; To install, save this somewhere and add the following to your .emacs file:
;;;
;;; (add-to-list 'load-path "/path/to/haml-mode.el")
;;; (require 'haml-mode nil 't)
;;; (add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
;;;

;;; Code:

(eval-when-compile (require 'cl))

;; User definable variables

(defgroup haml nil
  "Support for the Haml template language."
  :group 'languages
  :prefix "haml-")

(defcustom haml-mode-hook nil
  "Hook run when entering Haml mode."
  :type 'hook
  :group 'haml)

(defcustom haml-indent-offset 2
  "Amount of offset per level of indentation."
  :type 'integer
  :group 'haml)

(defcustom haml-backspace-backdents-nesting t
  "Non-nil to have `haml-electric-backspace' re-indent all code
nested beneath the backspaced line be re-indented along with the
line itself."
  :type 'boolean
  :group 'haml)

(defface haml-tab-face
  '((((class color)) (:background "hotpink"))
    (t (:reverse-video t)))
  "Face to use for highlighting tabs in Haml files."
  :group 'faces
  :group 'haml)

(defvar haml-indent-function 'haml-indent-p
  "This function should look at the current line and return true
if the next line could be nested within this line.")

(defvar haml-block-openers
  `("^ *\\([%\\.#][^ \t]*\\)\\(\\[.*\\]\\)?\\({.*}\\)?\\(\\[.*\\]\\)?[ \t]*$"
    "^ *[-=].*do[ \t]*\\(|.*|[ \t]*\\)?$"
    ,(concat "^ *-[ \t]*\\("
             (regexp-opt '("else" "elsif" "rescue" "ensure" "when"))
             "\\)")
    "^ */\\(\\[.*\\]\\)?[ \t]*$"
    "^ *-#"
    "^ *:")
  "A list of regexps that match lines of Haml that could have
text nested beneath them.")

;; Font lock

(defconst haml-font-lock-keywords
  '(("^ *\\(\t\\)"               1 'haml-tab-face)
    ("^!!!.*"                    0 font-lock-constant-face)
    ("\\('[^']*'\\)"             1 font-lock-string-face append)
    ("\\(\"[^\"]*\"\\)"          1 font-lock-string-face append)
    ("&?:\\w+"                   0 font-lock-constant-face append)
    ("@[a-z0-9_]+"               0 font-lock-variable-name-face append)
    ("| *$"                      0 font-lock-string-face)
    ("^[ \t]*\\(/.*\\)$"         1 font-lock-comment-face append)
    ("^ *\\(#[a-z0-9_]+\/?\\)"   1 font-lock-keyword-face)
    ("^ *\\(\\.[a-z0-9_]+\/?\\)" 1 font-lock-type-face)
    ("^ *\\(%[a-z0-9_]+\/?\\)"   1 font-lock-function-name-face)
    ("^ *\\(#[a-z0-9_]+\/?\\)"   (1 font-lock-keyword-face)
     ("\\.[a-z0-9_]+" nil nil    (0 font-lock-type-face)))
    ("^ *\\(\\.[a-z0-9_]+\/?\\)" (1 font-lock-type-face)
     ("\\.[a-z0-9_]+" nil nil    (0 font-lock-type-face)))
    ("^ *\\(\\.[a-z0-9_]+\/?\\)" (1 font-lock-type-face)
     ("\\#[a-z0-9_]+" nil nil    (0 font-lock-keyword-face)))
    ("^ *\\(%[a-z0-9_]+\/?\\)"   (1 font-lock-function-name-face)
     ("\\.[a-z0-9_]+" nil nil    (0 font-lock-type-face)))
    ("^ *\\(%[a-z0-9_]+\/?\\)"   (1 font-lock-function-name-face)
     ("\\#[a-z0-9_]+" nil nil    (0 font-lock-keyword-face)))
    ("^ *\\([~=-] .*\\)"         1 font-lock-preprocessor-face prepend)
    ("^ *[\\.#%a-z0-9_]+\\([~=-] .*\\)"     1 font-lock-preprocessor-face prepend)
    ("^ *[\\.#%a-z0-9_]+\\({[^}]+}\\)"      1 font-lock-preprocessor-face prepend)
    ("^ *[\\.#%a-z0-9_]+\\(\\[[^]]+\\]\\)"  1 font-lock-preprocessor-face prepend)))

;; Mode setup

(defvar haml-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table in use in haml-mode buffers.")

(defvar haml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [backspace] 'haml-electric-backspace)
    (define-key map "\C-?" 'haml-electric-backspace)
    (define-key map "\C-\M-f" 'haml-forward-sexp)
    (define-key map "\C-\M-b" 'haml-backward-sexp)
    (define-key map "\C-\M-u" 'haml-up-list)
    (define-key map "\C-\M-d" 'haml-down-list)
    (define-key map "\C-C\C-k" 'haml-kill-line-and-indent)
    map))

;;;###autoload
(define-derived-mode haml-mode fundamental-mode "Haml"
  "Major mode for editing Haml files.

\\{haml-mode-map}"
  (set-syntax-table haml-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'haml-indent-line)
  (set (make-local-variable 'indent-region-function) 'haml-indent-region)
  (set (make-local-variable 'forward-sexp-function) 'haml-forward-sexp)
  (setq indent-tabs-mode nil)
  (setq font-lock-defaults '((haml-font-lock-keywords) nil t)))

;; Navigation

(defun haml-forward-through-whitespace (&optional backward)
  "Move the point forward at least one line, until it reaches
either the end of the buffer or a line with no whitespace.

If `backward' is non-nil, move the point backward instead."
  (let ((arg (if backward -1 1))
        (endp (if backward 'bobp 'eobp)))
    (loop do (forward-line arg)
          while (and (not (funcall endp))
                     (looking-at "^[ \t]*$")))))

(defun haml-at-indent-p ()
  "Returns whether or not the point is at the first
non-whitespace character in a line or whitespace preceding that
character."
  (let ((opoint (point)))
    (save-excursion
      (back-to-indentation)
      (>= (point) opoint))))

(defun haml-forward-sexp (&optional arg)
  "Move forward across one nested expression.
With `arg', do it that many times.  Negative arg -N means move
backward across N balanced expressions.

A sexp in Haml is defined as a line of Haml code as well as any
lines nested beneath it."
  (interactive "p")
  (or arg (setq arg 1))
  (if (and (< arg 0) (not (haml-at-indent-p)))
      (back-to-indentation)
    (while (/= arg 0)
      (let ((indent (current-indentation)))
        (loop do (haml-forward-through-whitespace (< arg 0))
              while (and (not (eobp))
                         (not (bobp))
                         (> (current-indentation) indent)))
        (back-to-indentation)
      (setq arg (+ arg (if (> arg 0) -1 1)))))))

(defun haml-backward-sexp (&optional arg)
  "Move backward across one nested expression.
With ARG, do it that many times.  Negative arg -N means move
forward across N balanced expressions.

A sexp in Haml is defined as a line of Haml code as well as any
lines nested beneath it."
  (interactive "p")
  (haml-forward-sexp (if arg (- arg) -1)))

(defun haml-up-list (&optional arg)
  "Move out of one level of nesting.
With ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (let ((indent (current-indentation)))
      (loop do (haml-forward-through-whitespace t)
            while (and (not (bobp))
                       (>= (current-indentation) indent)))
      (setq arg (- arg 1))))
  (back-to-indentation))

(defun haml-down-list (&optional arg)
  "Move down one level of nesting.
With ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (let ((indent (current-indentation)))
      (haml-forward-through-whitespace)
      (when (<= (current-indentation) indent)
        (haml-forward-through-whitespace t)
        (back-to-indentation)
        (error "Nothing is nested beneath this line"))
      (setq arg (- arg 1))))
  (back-to-indentation))

(defun haml-mark-sexp-but-not-next-line ()
  "Marks the next Haml sexp, but puts the mark at the end of the
last line of the sexp rather than the first non-whitespace
character of the next line."
  (mark-sexp)
  (set-mark
   (save-excursion
     (goto-char (mark))
     (forward-line -1)
     (end-of-line)
     (point))))

;; Indentation and electric keys

(defun haml-indent-p ()
  "Returns true if the current line can have lines nested beneath it."
  (loop for opener in haml-block-openers
        if (looking-at opener) return t
        finally return nil))

(defun haml-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (bobp) 0
      (haml-forward-through-whitespace t)
      (+ (current-indentation)
         (if (funcall haml-indent-function) haml-indent-offset
           0)))))

(defun haml-indent-region (start end)
  "Indent each nonblank line in the region.
This is done by indenting the first line based on
`haml-compute-indentation' and preserving the relative
indentation of the rest of the region.

If this command is used multiple times in a row, it will cycle
between possible indentations."
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (let (this-line-column current-column
          (next-line-column
           (if (and (equal last-command this-command) (/= (current-indentation) 0))
               (* (/ (- (current-indentation) 1) haml-indent-offset) haml-indent-offset)
             (haml-compute-indentation))))
      (while (< (point) end)
        (setq this-line-column next-line-column
              current-column (current-indentation))
        ;; Delete whitespace chars at beginning of line
        (delete-horizontal-space)
        (unless (eolp)
          (setq next-line-column (save-excursion
                                   (loop do (forward-line 1)
                                         while (and (not (eobp)) (looking-at "^[ \t]*$")))
                                   (+ this-line-column
                                      (- (current-indentation) current-column))))
          ;; Don't indent an empty line
          (unless (eolp) (indent-to this-line-column)))
        (forward-line 1)))
    (move-marker end nil)))

(defun haml-indent-line ()
  "Indent the current line.
The first time this command is used, the line will be indented to the
maximum sensible indentation.  Each immediately subsequent usage will
back-dent the line by `haml-indent-offset' spaces.  On reaching column
0, it will cycle back to the maximum sensible indentation."
  (interactive "*")
  (let ((ci (current-indentation))
        (cc (current-column))
        (need (haml-compute-indentation)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (if (and (equal last-command this-command) (/= ci 0))
          (indent-to (* (/ (- ci 1) haml-indent-offset) haml-indent-offset))
        (indent-to need)))
      (if (< (current-column) (current-indentation))
          (forward-to-indentation 0))))

(defun haml-reindent-region-by (n)
  "Add N spaces to the beginning of each line in the region.
If N is negative, will remove the spaces instead.  Assumes all
lines in the region have indentation >= that of the first line."
  (let ((ci (current-indentation)))
    (save-excursion
      (replace-regexp (concat "^" (make-string ci ? ))
                      (make-string (max 0 (+ ci n)) ? )
                      nil (point) (mark)))))

(defun haml-electric-backspace (arg)
  "Delete characters or back-dent the current line.
If invoked following only whitespace on a line, will back-dent
the line and all nested lines to the immediately previous
multiple of `haml-indent-offset' spaces.

Set `haml-backspace-backdents-nesting' to nil to just back-dent
the current line."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
          (bolp)
          (looking-at "^[ \t]+$"))
      (backward-delete-char arg)
    (let ((ci (current-column)))
      (beginning-of-line)
      (if haml-backspace-backdents-nesting
          (haml-mark-sexp-but-not-next-line)
        (set-mark (save-excursion (end-of-line) (point))))
      (haml-reindent-region-by (* (- arg) haml-indent-offset))
      (back-to-indentation)
      (pop-mark))))

(defun haml-kill-line-and-indent ()
  "Kill the current line, and re-indent all lines nested beneath it."
  (interactive)
  (beginning-of-line)
  (haml-mark-sexp-but-not-next-line)
  (kill-line 1)
  (haml-reindent-region-by (* -1 haml-indent-offset)))

;; Setup/Activation

(provide 'haml-mode)
