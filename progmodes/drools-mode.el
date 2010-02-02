;;; Copyright Philip Dorrell 2008

;;; This file supplies an GNU Emacs editing mode for
;;; Drools, the open source Java-based rules engine.
;;; See http://www.jboss.org/drools/ for details of the Drools project.

;;; Author Philip Dorrell. Email: http://www.1729.com/email.html

;;; Version Date: 27 Oct 2008

;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;; THE SOFTWARE.

(defun regexp-for-keyword-at-start-of-line (keywords)
  "Create a regexp to match one of a keyword from KEYWORDS at beginning of line, 
possibly preceded by whitespace."
  (concat "[ \t]*" (regexp-for-keywords keywords)) )

(defun regexp-for-keywords (keywords)
  "Create a regexp to match one of a keyword from KEYWORDS."
  (concat "\\<" (regexp-opt keywords t) "\\>") )

(defvar drools-builtin-keywords  '("modify" "insert" "insertLogical"
				   "retract" "update" "not")
  "Drools commands and operators occuring within actions.")

(defvar drools-toplevel-keywords 
  '("rule" "end" "package" "import" "function" "global" "expander")
  "Drools keywords that occur at top-level in source file, and should always be indented to 0")

(defvar drools-section-keywords '("when" "then")
  "Drools keywords which are the headings for sections of a rule.")

(defvar drools-attribute-keywords '("duration" "salience" "no-loop" "lock-on-active" "dialect"
				    "agenda-group" "auto-focus" "ruleflow-group")
  "Drools attributes that occur within a rule (before the sections).")

(defvar drools-toplevel-keyword-regexp 
  (regexp-for-keyword-at-start-of-line drools-toplevel-keywords)
  "Regexp to match a line starting with a top-level Drools keyword" )

(defvar drools-section-keyword-regexp 
  (regexp-for-keyword-at-start-of-line (append drools-section-keywords)) 
  "Regexp to match a line starting with a Drools section keyword" )

(defvar drools-indenting-keyword-regexp (regexp-for-keywords '("rule" "when" "then"))
  "Regexp for Drools keywords which cause indent on following lines.")

(defvar drools-tab-indent 4 "Default indentation for Drools mode")

(defvar drools-font-lock-keywords
  (list
   (cons (regexp-for-keywords drools-builtin-keywords)
	 'font-lock-builtin-face)
   (cons (regexp-for-keywords drools-toplevel-keywords)
	 'font-lock-keyword-face)
   (cons (regexp-for-keywords drools-section-keywords)
	 'font-lock-keyword-face)
   (cons (regexp-for-keywords drools-attribute-keywords)
	 'font-lock-keyword-face) )
  "Default highlighting expressions for Drools mode") 

(defvar drools-mode-syntax-table
  (let ((drools-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" drools-mode-syntax-table)
    (modify-syntax-entry ?- "w" drools-mode-syntax-table)
    (modify-syntax-entry ?/ ". 124b" drools-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" drools-mode-syntax-table)
    (modify-syntax-entry ?# "< b" drools-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" drools-mode-syntax-table)
    drools-mode-syntax-table
    )
  "Syntax table for Drools mode")

(defvar drools-mode-hook nil "Hook function for Drools mode")

(defun at-start-of-line()
  "Return t if we are at the start of a line"
  (eql (current-column) 0) )

(defun skip-backward-sexp-and-space()
  "Skip backwards a SEXP, and any preceding whitespace on the same line"
  (backward-sexp)(skip-chars-backward " \t") )

(defun drools-skip-to-previous-line()
  "Skip to previous line in Drools code which is at the same syntax level as the current line.
(Information about this position will be used to relatively indent the current line.)
If an opening bracket is hit, return a pair of the bracket and if there was any non-whitespace
after the bracket and sit at the start of that text. If not, move to first text on line, and return nil."
  (interactive)
  (beginning-of-line)
  (condition-case nil
      (progn
	(skip-backward-sexp-and-space)
	(while (not (at-start-of-line))
	  (skip-backward-sexp-and-space) )
	(skip-chars-forward " \t")
	nil)
    (scan-error 
     (let (bracket)
       (skip-chars-backward " \t\n") 
       (setq bracket (char-to-string (char-after (1- (point)))))
       (let ( (text-after-bracket (not (looking-at "[ \t]*$"))) )
	 (if text-after-bracket
	     (skip-chars-forward " \t") )
	 (cons bracket text-after-bracket) ) ) ) ) )

(defun drools-indent-line()
  "Indent line function for Drools mode"
  (interactive)
  (beginning-of-line)
  (cond 
   ((looking-at drools-toplevel-keyword-regexp) ;; top-level keywords, absolute indent to 0
    (indent-line-to 0))
   ((looking-at drools-section-keyword-regexp)  ;; section-level keywords, absolute indent to 1 tab width
    (indent-line-to drools-tab-indent))
   (t ;; All others, relatively indent to "previous line".
    (let (new-indent
	  (unindent (if (looking-at "[ \t]*[}]") (* -1 drools-tab-indent) 0)) ) ;; extra out-dent for closing }
      (save-excursion
	(let* ( (bracket-and-text (drools-skip-to-previous-line))
		(bracket (car bracket-and-text))
		(text-after-bracket (cdr bracket-and-text)) )
	  (if bracket-and-text
	    (cond
	      ((or (not text-after-bracket) (equal bracket "{"))
	       (beginning-of-line)
	       (skip-chars-forward " \t")
	       (setq new-indent (+ drools-tab-indent (current-column))) )
	      ((or (equal bracket "(") (equal bracket "["))
	       (setq new-indent (current-column)))
	      (t (setq new-indent (current-column))) )
	    (cond
	     ((looking-at drools-indenting-keyword-regexp)
	      (setq new-indent (+ drools-tab-indent (current-column))) )
	     (t (setq new-indent (current-column))) ) ) ) )
      (setq new-indent (+ unindent new-indent))
      (if (< new-indent 0)
	  (progn 
	    (message "Invalid syntax, cannot indent further left than margin")
	    (setq new-indent 0) ) )
      (indent-line-to new-indent) ) ) ) )

(define-derived-mode drools-mode fundamental-mode "Drools"
  "Major mode for editing Drools files."
  (set (make-local-variable 'font-lock-defaults) '(drools-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'drools-indent-line)
  (set (make-local-variable 'parse-sexp-ignore-comments) t ) )
