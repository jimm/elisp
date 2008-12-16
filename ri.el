;;; ri.el -- emacs support for Dave Thomas's "ri" (Ruby Index) utility
;;
;; Author: David Alan Black (dblack@candle.superlink.net)
;; Version: 0.1.0
;;
;; Revision info:
;; $Id: ri.el,v 1.17 2001/03/15 11:49:36 dab Exp $
;; $Author: dab $
;; $Date: 2001/03/15 11:49:36 $
;;
;; Copyright (c) David Alan Black, 2001
;; This software is distributed on the same terms as Ruby itself.
;;
;; Thanks to Dave Thomas for writing ri, and for facilitating the
;; creation of this software.


;;; Commentary:
;;
;; This commentary includes:
;;
;;   Summary
;;   Installation and setup
;;   Usage and examples
;;   To do/fix
;;   ri-related notes
;;
;;
;; Summary:
;;
;; ri.el offers Emacs integration of the ri (Ruby Index) utility.  You
;; can query ri directly, navigate around ri screens using Ruby names
;; as links, and get instant ri documentation on any Ruby name at
;; point, in any buffer.
;;
;;
;; Installation and setup:
;;
;; (Note: this describes a standard, permanent way of installing ri.
;;  You may, of course, want to change keybindings, or use a subset
;;  of the configuration described here.)
;;
;; 1. Make sure you have ri running.  You also need the file Emacs.rb
;; (which should be distributed with this file).
;;
;; 2. Put this file, ri.el, somewhere where emacs will see it.  Put
;; Emacs.rb either in the site-wide place for ri output format files,
;; or somewhere else which you will then specify in your ~/.emacs (see
;; below).
;;
;; 3. Put the following block of lines (between the horizontal lines)
;; in your ~/.emacs, or in a separate start-up file read by your
;; ~/.emacs.  Take off the first two semicolons of each line.  The
;; remaining comments will guide you in customizing the lines.
;;
;;;;; ----------------------------------------------------------
;;;;; Support for ri, Dave Thomas's Ruby Index utility,
;;;;; by David Alan Black
;;;;;
;; (autoload 'ruby-index "ri.el" "ri utility" t)
;; (autoload 'ri "ri.el" "ri utility" t)
;; (autoload 'ri-show-term-at-point "ri.el" "ri utility" t)
;; (autoload 'ri-show-term-composite-at-point "ri.el" "ri utility" t)
;;
;;;; Set this command to whatever you use to run ri:
;; (setq ri-ri-command "/usr/local/bin/ri")
;;
;;;; If you put Emacs.rb somewhere other than the main ri/op
;;;; directory, then change "Emacs" to "/full/path/to/Emacs.rb":
;; (setq ri-emacsrb "Emacs")
;;
;;;; These two global key bindings enable the instant ri lookup of a
;;;; term at point.  
;; (global-set-key "\C-c\C-c\C-r" 'ri-show-term-at-point)
;; (global-set-key "\C-c\C-c\C-t" 'ri-show-term-composite-at-point)
;;;;
;;;; Keybindings for buffer navigation inside an ri output buffer.
;;;; Uncomment and change if desired.
;;;;
;;; (setq ri-key-stcap "\C-m")
;;; show-term-composite at point (same as global above, but bound to
;;; <ENTER> for convenience)
;;;
;;; (setq ri-key-quit "q")
;;; bound to function ri-quit, which is probably quit-window)
;;;
;;; (setq ri-key-fnt "\C-i")
;;; find-next-term (TAB)
;;;
;;; (setq ri-key-fpt "\M-\C-i")
;;; find-previous-term (META-TAB)
;;;
;;;; Miscellaneous boolean configuration variables.  Change and
;;;; uncomment if you wish.  Asterisk means default.
;;;
;;; (setq ri-use-one-buffer t)
;;; *t:   put all ri output in a single buffer
;;; nil: create a different buffer for each call to ri
;;;
;;; (setq ri-overwrite-buffer nil)
;;; t:   replace ri buffer contents with output from later calls to ri
;;; *nil: put new output at top of buffer leaving old output in place
;;;
;;; (setq ri-switch-to-ri-buffer t)
;;; *t:   select ri output buffer automatically after call to ri
;;; nil: show ri output buffer but don't select it
;;;
;;; (setq ri-read-only t)
;;; *t:   ri output buffer(s) is (are) read-only 
;;; nil: ri output buffer(s) is (are) read/write

;;;;; end of ri support
;;;;; -----------------------------------------------------------
;;
;;
;; Usage and examples:
;;
;; (Note: see Installation, above, for notes on configurable things in
;; ri.el.)
;;
;; There are three ways to query ri.  Whichever you use, once you are
;; in an ri output buffer, you can tab around and hit [enter] to get
;; ri info on the term at point.
;;
;; 1. Querying ri directly:
;;
;; The interactive function 'ri' (M-x ri) will prompt you for a
;; term, and will show you the ri output for what you enter.
;;
;; Hint: if you give ri an empty term (just hit enter when prompted),
;; you'll get a buffer with every documented method!
;;
;; 2. Using the index buffer:
;;
;; You can start up an index buffer with "M-x ruby-index" This will
;; give you a list of all documented classes and modules.  In this
;; buffer, [TAB] advances you by word (actually by possibly-valid Ruby
;; index term -- it doesn't know "map" from "maple", so bear with it
;; if it seems to stop on too many words).  Use M-TAB (alt-tab,
;; generally) to go back one term.  Hit <enter> on any term to see its
;; documentation (or an error message, if it isn't a valid term).
;;
;; 3. Getting pop-up ri info on the term at point (in any buffer):
;;
;; The most powerful feature of the ri/emacs interface is the ability
;; to summon documentation on any Ruby class, module, or method with
;; one keystroke.  You do not have to be in a special buffer to do
;; this.  You can be reading "War and Peace", for all ri.el cares,
;; when you ask to see the documentation on "send" or "collect".
;;
;; As shipped (if you put the above block in your ~/.emacs), ri.el
;; binds two keys for this purpose.  The behavior of these keys is
;; illustrated in the following examples.  (The caret below each
;; sample text line indicates the character after point.)
;;
;; Example 1:
;;
;;     Text: Into each life a little rain must fall.
;;                 ^
;;     Hit "\C-c\C-c\C-r", and you'll get documentation for 'each'.
;;     Note that point does not have to be at the beginning of the
;;     term.
;;
;; Example 2:
;;
;;     Text: Here's a sentence in which Array#each occurs.
;;                                        ^
;;     Hit "\C-c\C-c\C-r" and you'll get documentation for "Array".
;;     Hit "\C-c\C-c\C-t" and you'll get documentation for "Array#each".
;;    
;;     The "\C-r" version of the command stops at the separators
;;     [#:.].  The "\C-t" version includes those separators, and
;;     whatever valid characters come after them, as part of the term.
;;
;;     (Note: [enter] in an ri output buffer is bound to
;;     \C-c\C-c\C-t, the "greedy" version.)
;;
;; Example 3: (semi-bug)
;;
;;     Text: Here's a sentence that ends with Array#each.
;;                                            ^
;;     If you do "\C-c\C-c\C-t" here, you're asking for documentation
;;     on Array#each. (including the period), which will give you an
;;     error.  So if you get told that something doesn't exist when
;;     you thought it would, make sure it hasn't grabbed an extra
;;     character.
;; 
;;
;; To do/fix:
;;
;; If you're in (say) the buffer for Array and you select a method
;; that's unique to Array (say, flatten), you'll get the correct ri
;; entry but the buffer name will be wrong (collect instead of
;; Array#collect).  This is only a problem if you're not using the
;; one-buffer approach.
;;
;; If, again in an Array buffer, you select a method that isn't unique
;; to Array (say, each), you'll get the general "each is not a unique
;; name" screen, not Array#each.  You can then select Array#each --
;; it's just a bit of a detour.  (But maybe you *wanted* the general
;; "each" screen :-)
;;
;;
;; ri-related notes:
;;
;; ri exit values:
;;
;; 0 normal
;; 1 couldn't find class
;; 2 usage (rendered here as synopsis (-s))
;; 3 couldn't find method (describeMethod)
;; 4 couldn't find method (findAndDescribe)

;;; Code:

(require 'thingatpt)

;;; Variables

;; These *can*, but probably should not be, overridden:

(defvar ri-char-re "[]\[A-Za-z_?!=+<>]")
(defvar ri-term-re (concat "\\(" ri-char-re "+\\)"))
(defvar ri-char-composite-re "[]\[A-Za-z#.:_?!=+<>]")
(defvar ri-term-composite-re (concat "\\(" ri-char-composite-re "+\\)"))
(defvar ri-one-buffer-name "Ruby Index (ri) output")

;; These are freely configurable (see Commentary)

;; Booleans
(defvar ri-use-one-buffer t)
(defvar ri-overwrite-buffer nil)
(defvar ri-switch-to-ri-buffer t)
(defvar ri-read-only t)

;; Key bindings
(defvar ri-key-stcap "\C-m")
(defvar ri-key-quit "q")
(defvar ri-key-fnt "\C-i")
(defvar ri-key-fpt "\M-\C-i")


;;; Main entry point and other high-level functions

(defun ri-process(&optional term)
  "Main."
  (interactive)
  (or
   (ri-redisplay term)
   (ri-first-display (or term "--synopsis"))))

(defun ri-first-display(term)
  "Call ri, creating or reusing a buffer."
  (let* ((scratch (ri-new-scratch-buffer))
	 (status (ri-ri-run term scratch)))
    (if	(ri-error status)
	(ri-handle-error scratch)
      (let ((buffer (ri-buffer term)))
	(cond ((ri-usage status) (ri-handle-usage scratch buffer))
	      (t (ri-handle-normal scratch buffer)))))))

(defun ri-redisplay(term)
  "Display the term-specific output buffer, if any, or return nil."
  (if (ri-on-display-p term)
      (ri-display-buffer (ri-buffer term))
    nil))

(defun ri-display-buffer(ribuffer)
  "Final stage in actually showing an ri output buffer,
including setting up the key bindings."
  (or
   (eq (current-buffer) ribuffer)
   (progn
     (save-excursion
       (set-buffer ribuffer)
       (and ri-read-only
	    (setq buffer-read-only t))
       (local-set-key ri-key-quit 'ri-quit)
       (local-set-key ri-key-stcap 'ri-show-term-composite-at-point)
       (local-set-key ri-key-fnt 'ri-find-next-term)
       (local-set-key ri-key-fpt 'ri-find-previous-term))
     (and ri-switch-to-ri-buffer
	  (switch-to-buffer-other-window ribuffer))))
  (goto-char (point-min)))

(defun ri-on-display-p(term)
  "True if an output buffer for term exists."
  (buffer-live-p (get-buffer (ri-buffer-name term))))

;;;  Mid-low-level manipulation of buffer names and buffer creation

(defun ri-one-buffer()
  "Return the sole output buffer when ri-use-one-buffer is true"
  (get-buffer-create ri-one-buffer-name))

(defun ri-buffer-name(&optional term)
  "Return the name of the sole buffer, or a name based on term,
depending on whether ri-use-one-buffer is true"
  (if (or
       ri-use-one-buffer
       (not (stringp term)))
      ri-one-buffer-name
    (concat "**Ruby Index: " term "**")))

(defun ri-buffer(&optional term)
  "Returns the sole buffer or a buffer based on name, depending on
whether ri-use-one-buffer is true"
  (if ri-use-one-buffer
      (ri-one-buffer)
      (get-buffer-create(ri-buffer-name term))))

(defun ri-error-buffer()
  "Returns the ri error message buffer"
  (get-buffer-create "**Ruby Index: indexing error**"))

(defun ri-new-scratch-buffer()
  "Returns a uniquely-named scratch buffer"
  (generate-new-buffer "__riscratch__"))

;;; Handlers for normal, "usage", and error results from ri.

(defun ri-handle-normal(scratch buffer)
  "Handle normal/successful ri queries"
    (ri-flush-scratch scratch buffer)
    (ri-display-buffer buffer))

(defun ri-handle-usage(scratch buf)
  "Handle requests with empty terms.  Based on the 'ri --synopsis'
usage message"
  (save-excursion
    (set-buffer buf)
    (insert "The following classes and modules are documented:\n\n"))
  (ri-handle-normal scratch buf))

(defun ri-handle-error(scratch)
  "Handle abnormally exiting calls to ri."
  (let ((ebuf (ri-error-buffer)))
    (save-excursion
      (set-buffer ebuf)
      (display-buffer ebuf)
      (goto-char (point-max))
      (insert-buffer scratch))
    (kill-buffer scratch)))

;;; Transfer contents of scratch buffer to real buffer

(defun ri-flush-scratch(scratch buffer)
  "Copy and kill the scratch (ri output) buffer"
  (save-excursion
    (set-buffer buffer)
    (if ri-overwrite-buffer
	(erase-buffer))
    (goto-char (point-min))
    (insert-buffer scratch)
    (kill-buffer scratch)))

(defun ri-error(code)
  "ri exit status codes for non-normal exit"
  (member code '(1 3 4)))

(defun ri-usage(code)
  "ri exit status code for usage/synopsis"
  (= code 2))

(defun ri-quit()
  "Function bound to 'q' key in ri output buffer(s)"
  (interactive)
  (quit-window))

;;; Functions to find and show terms at or beyond point

(defun ri-term-at-point(termre)
  "Return the (potential) Ruby/ri term at point, or nil."
  (interactive)
  (thing-at-point-looking-at termre)
  (match-string 1))

(defun ri-find-next-term()
  "Forward search for next (potential) Ruby/ri term."
  (interactive)
  (while (looking-at ri-term-re)
    (forward-char))
  (re-search-forward ri-term-re)
  (goto-char (match-beginning 0)))

(defun ri-find-previous-term()
  "Backward search for last (potential) Ruby/ri term."
  (interactive)
  (while (looking-at ri-term-re)
    (backward-char))
  (re-search-backward ri-term-re)
  (while (looking-at ri-term-re)
    (backward-char))
  (goto-char (match-beginning 0)))

(defun ri-show-term-at-point()
  "Show ri documentation for the term at point.  Term stops
at separator characters (#.:)"
  (interactive)
  (ri-process(ri-term-at-point ri-term-re)))

(defun ri-show-term-composite-at-point()
  "Show ri documentation for the term at point.  Term can include
separator characters (#.:)"
  (interactive)
  (ri-process(ri-term-at-point ri-term-composite-re)))

;;; High-level interface functions

(defun ri(term)
  "Simple interactive interface to ri."
  (interactive "sTerm to document: ")
  (ri-process term))

(defun ruby-index()
  "Triggers the display of the top-level index page."
  (interactive)
  (ri-process))

;;; Run ri

(defun ri-ri-run(term buf)
  "Run ri"
  (call-process ri-ri-command nil buf nil "-f" ri-emacsrb term))


;;; end of ri.el
