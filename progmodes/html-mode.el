;;; --------------------------------------------------------------------------
;;; HTML mode, based on text mode.
;;; Copyright (C) 1985 Free Software Foundation, Inc.
;;; Copyright (C) 1992, 1993 National Center for Supercomputing Applications.
;;; NCSA modifications by Marc Andreessen (marca@ncsa.uiuc.edu).
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 1, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; -------------------------------- CONTENTS --------------------------------
;;;
;;; html-mode: Major mode for editing HTML hypertext documents.
;;; Revision: 2.0 (beta)
;;;
;;; ------------------------------ INSTRUCTIONS ------------------------------
;;;
;;; Put the following code in your .emacs file:
;;;
;;; (autoload 'html-mode "html-mode" "HTML major mode." t)
;;; (or (assoc "\\.html$" auto-mode-alist)
;;;   (setq auto-mode-alist (cons '("\\.html$" . html-mode) 
;;;                               auto-mode-alist)))
;;;
;;; Emacs will detect the ``.html'' suffix and activate html-mode
;;; appropriately.
;;;
;;; You are assumed to be at least somewhat familiar with the HTML
;;; format.  If you aren't, read about it first (see below).
;;;
;;; Here are key sequences and corresponding commands:
;;;
;;; NORMAL COMMANDS:
;;;
;;; C-c a         html-add-address
;;;   Open an address element.
;;;
;;; C-c b         html-add-blockquote
;;;
;;; C-c C-b       html-add-bold
;;;   Open a bold element.
;;;
;;; C-c c         html-add-code
;;;   Open a 'code' (fixed-font) element.
;;;
;;; C-c C-c       html-add-citation
;;;
;;; C-c d         html-add-description-list
;;;   Open a definition list.  The initial entry is created for you.
;;;   To create subsequent entries, use 'C-c e'.
;;;
;;; C-c e         html-add-description-entry
;;;   Add a new definition entry in a definition list.  You are
;;;   assumed to be inside a definition list (specifically, at the end
;;;   of another definition entry).
;;;
;;; C-c C-e       html-add-emphasized
;;;   Open an emphasized element.
;;;
;;; C-c C-f       html-add-fixed
;;;
;;; C-c g         html-add-img
;;;   Add an IMG element (inlined image or graphic).  Note that the
;;;   IMG tag is currently an extension to HTML supported only by the
;;;   NCSA Mosaic browser (to my knowledge).  You will be prompted for
;;;   the URL of the image you wish to inline into the document.
;;;
;;; C-c h         html-add-header
;;;   Add a header.  You are prompted for size (1 is biggest, 2 is
;;;   next biggest; bottom limit is 6) and header contents.
;;;
;;; C-c i         html-add-list-or-menu-item
;;;   Add a new list or menu item in a list or menu.  You are assumed
;;;   to be inside a list or menu (specifically, at the end of another
;;;   item).
;;;
;;; C-c C-i       html-add-italic
;;;   Open an italic element.
;;;
;;; C-c C-k       html-add-keyboard
;;;
;;; C-c l         html-add-normal-link
;;;   Add a link.  You will be prompted for the link (any string;
;;;   e.g., http://foo.bar/argh/blagh).  The cursor will be left where
;;;   you can type the text that will represent the link in the
;;;   document.
;;;
;;; C-c C-l       html-add-listing
;;;
;;; C-c m         html-add-menu
;;;   Open a menu.  The initial item is created for you.  To create
;;;   additional items, use 'C-c i'.
;;;
;;; C-c C-m       html-add-sample
;;;
;;; C-c n         html-add-numbered-list
;;;
;;; C-c p         html-add-paragraph-separator
;;;   Use this command at the end of each paragraph.
;;; 
;;; C-c C-p       html-add-preformatted
;;;
;;; C-c r         html-add-normal-reference
;;;
;;; C-c s         html-add-list
;;;   Open a list.  The initial item is created for you.  To create
;;;   additional items, use 'C-c i'.
;;;
;;; C-c C-s       html-add-strong
;;;
;;; C-c t         html-add-title
;;;   Add a title to the document.  You will be prompted for the
;;;   contents of the title.  If a title already exists at the very
;;;   top of the document, the existing contents will be replaced.
;;;
;;; C-c C-v       html-add-variable
;;;
;;; C-c x         html-add-plaintext
;;;   Add plaintext.  The cursor will be positioned where you can type
;;;   plaintext (or insert another file, or whatever).
;;;
;;; C-c z         html-preview-document
;;;   Fork off a Mosaic process to preview the current document.
;;;   After you do this once, subsequent invocations of html-preview-document
;;;   will cause the same Mosaic process to be used; this magic is
;;;   accomplished through Mosaic's ability to be remote-controlled
;;;   via Unix signals.  This feature is only available when running
;;;   Lucid Emacs v19 (it will maybe work with GNU Emacs v19; I'm not
;;;   sure).
;;;
;;; COMMANDS THAT OPERATE ON THE CURRENT REGION:
;;;
;;; C-c C-r l     html-add-normal-link-to-region
;;;   Add a link that will be represented by the current region.  You
;;;   will be prompted for the link (any string, as with
;;;   html-add-normal-link).
;;;
;;; C-c C-r r     html-add-reference-to-region
;;;   Add a reference (a link that does not reference anything) that
;;;   will be represented by the current region.  You will be prompted
;;;   for the name of the link; if you just press RET, a numeric name
;;;   will be created for you.
;;;
;;; SPECIAL COMMANDS:
;;;
;;; <, >, &
;;;   These are overridden to output &lt;, &gt;, and &amp;
;;;   respectively.  The real characters <, >, and & can be entered
;;;   into the text either by typing 'C-c' before typing the character
;;;   or by using the Emacs quoted-insert (C-q) command.
;;;
;;; C-c <, C-c >, C-c &
;;;   See '<, >, &' above.
;;;
;;; ---------------------------- ADDITIONAL NOTES ----------------------------
;;;
;;; If you are running Epoch or Lucid Emacs, highlighting will be used
;;; to deemphasize HTML message elements as they are created.  You can
;;; turn this off; see the variable 'html-use-highlighting'.
;;;
;;; To reorder all of the link NAME fields in your message (in order
;;; of their occurrence in the text), use:
;;;
;;; html-reorder-numeric-names
;;;   Reorder the NAME fields for links in the current buffer.  The
;;;   new ordering starts at 1 and increases monotonically through the
;;;   buffer.  If optional arg REORDER-NON-NUMERIC is non-nil, then
;;;   non-numeric NAME's will also be numbered, else they won't.
;;;
;;; HREF arguments in anchors should always be quoted.  In some
;;; existing HTML documents, they are not.  html-mode will
;;; automatically quotify all such unquoted arguments when it
;;; encounters them.  The following variables affect this behavior.
;;;
;;; html-quotify-hrefs-on-find       (variable, default t)
;;;   If this is non-nil, all HREF arguments will be quotified
;;;   automatically when a HTML document is loaded into Emacs
;;;   (actually when html-mode is entered).
;;;
;;; -------------------------------- GOTCHAS ---------------------------------
;;;
;;; HTML documents can be tricky.  html-mode is not smart enough to
;;; enforce correctness or sanity, so you have to do that yourself.
;;;
;;; In particular, html-mode is smart enough to generate unique
;;; numeric NAME id's for all links that were (1) created via an
;;; html-mode command or (2) present in the file when it was loaded.
;;; Any other links (e.g. links added via Emacs cut and paste) may
;;; have ID's that conflict with ID's html-mode generates.  You must
;;; watch for this and fix it when appropriate; otherwise, your
;;; hypertext document will not work correctly under some browsers.
;;;
;;; html-reorder-numeric-names can be used to reset all of the NAME
;;; id's in a document to an ordered sequence; this will also give
;;; html-mode a chance to look over the document and figure out what
;;; new links should be named to be unique.  However, note that doing
;;; so may confuse references to named anchors from other HTML
;;; documents.  Beeeeeeee careful.
;;;
;;; ------------------------- WHAT HTML-MODE IS NOT --------------------------
;;;
;;; html-mode is not a mode for *browsing* HTML documents.  In
;;; particular, html-mode provides no hypertext or World Wide Web
;;; capabilities.
;;;
;;; The World Wide Web browser we (naturally) recommend is NCSA
;;; Mosaic, which can be found at ftp.ncsa.uiuc.edu in /Mosaic.
;;;
;;; See file://moose.cs.indiana.edu/pub/elisp/w3 for w3.el, which is
;;; an Elisp World Wide Web browser written by William Perry.
;;;
;;; ------------------------------ WHAT HTML IS ------------------------------
;;;
;;; HTML (HyperText Markup Language) is a format for hypertext
;;; documents, particularly in the World Wide Web system.  For more
;;; information on HTML, telnet to info.cern.ch or pick up a copy of
;;; NCSA Mosaic for the X Window System via ftp to ftp.ncsa.uiuc.edu
;;; in /Mosaic; information is available online through the software
;;; products distributed at those sites.
;;;
;;; ---------------------------- ACKNOWLEDGEMENTS ----------------------------
;;;
;;; Some code herein provided by:
;;;   Dan Connolly <connolly@pixel.convex.com>
;;;
;;; --------------------------------------------------------------------------
;;; LCD Archive Entry:
;;; html-mode|Marc Andreessen|marca@ncsa.uiuc.edu|
;;; Major mode for editing HTML hypertext files.|
;;; 17-Jul-1993|2.0 beta|~/modes/html-mode.el.Z|
;;; --------------------------------------------------------------------------

;;; ---------------------------- emacs variations ----------------------------

(defvar html-running-lemacs (string-match "Lucid" emacs-version)
  "Non-nil if running Lucid Emacs.")

(defvar html-running-epoch (boundp 'epoch::version)
  "Non-nil if running Epoch.")

;;; ------------------------------- variables --------------------------------

(defvar html-quotify-hrefs-on-find t
  "*If non-nil, all HREF's in a file will be automatically quotified
when the file is loaded.  This is useful for converting ancient HTML
documents to SGML-compatible syntax, which mandates quoted HREF's.
This should always be T.")

(defvar html-use-highlighting html-running-epoch
  "*Flag to use highlighting for HTML directives in Epoch or Lucid Emacs; 
if non-NIL, highlighting will be used.  Default is T if you are running
Epoch; nil otherwise (for Lucid Emacs, font-lock is better; see 
html-use-font-lock instead).")

(defvar html-use-font-lock html-running-lemacs
  "*Flag to use font-lock for HTML directives in Lucid Emacs.  If non-NIL,
font-lock will be used.  Default is T if you are running with Lucid Emacs;
NIL otherwise.")

(defvar html-give-anchors-numeric-names nil
  "*Flag to indicate whether new anchors should be given numeric names
by default.  If non-NIL, they will be.")

(defvar html-deemphasize-color "grey80"
  "*Color for de-highlighting HTML directives in Epoch or Lucid Emacs.")

(defvar html-emphasize-color "yellow"
  "*Color for highlighting HTML something-or-others in Epoch or Lucid Emacs.")

(defvar html-document-previewer "/usr/local/bin/xmosaic"
  "*Program to be used to preview HTML documents.  Program is assumed
to accept a single argument, a filename containing a file to view; program
is also assumed to follow the Mosaic convention of handling SIGUSR1 as
a remote-control mechanism.")

(defvar html-document-previewer-args "-ngh"
  "*Arguments to be given to the program named by html-document-previewer;
NIL if none should be given.")

(defvar html-sigusr1-signal-value 16
  "*Value for the SIGUSR1 signal on your system.  See, usually,
/usr/include/sys/signal.h.")

;;; --------------------------------- setup ----------------------------------

(defvar html-mode-syntax-table nil
  "Syntax table used while in html mode.")

(defvar html-mode-abbrev-table nil
  "Abbrev table used while in html mode.")
(define-abbrev-table 'html-mode-abbrev-table ())

(if html-mode-syntax-table
    ()
  (setq html-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" ".   " html-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " html-mode-syntax-table)
  (modify-syntax-entry ?' "w   " html-mode-syntax-table))

(defvar html-mode-map nil "")
(if html-mode-map
    ()
  (setq html-mode-map (make-sparse-keymap))
  (define-key html-mode-map "\t" 'tab-to-tab-stop)
  (define-key html-mode-map "\C-ca" 'html-add-address)
  (define-key html-mode-map "\C-cb" 'html-add-blockquote)
  (define-key html-mode-map "\C-cc" 'html-add-code)
  (define-key html-mode-map "\C-cd" 'html-add-description-list)
  (define-key html-mode-map "\C-ce" 'html-add-description-entry)
  (define-key html-mode-map "\C-cg" 'html-add-img)
  (define-key html-mode-map "\C-ch" 'html-add-header)
  (define-key html-mode-map "\C-ci" 'html-add-list-or-menu-item)
  (define-key html-mode-map "\C-cl" 'html-add-normal-link)
  (define-key html-mode-map "\C-cm" 'html-add-menu)
  (define-key html-mode-map "\C-cn" 'html-add-numbered-list)
  (define-key html-mode-map "\C-cp" 'html-add-paragraph-separator)
  (define-key html-mode-map "\C-cr" 'html-add-normal-reference)
  (define-key html-mode-map "\C-cs" 'html-add-list)
  (define-key html-mode-map "\C-ct" 'html-add-title)
  (define-key html-mode-map "\C-cx" 'html-add-plaintext)
  ;; html-preview-document currently requires the primitive
  ;; signal-process, which is only in v19 (is it in gnu 19? dunno).
  (and html-running-lemacs
       (define-key html-mode-map "\C-cz" 'html-preview-document))
  (define-key html-mode-map "\C-c\C-b" 'html-add-bold)
  (define-key html-mode-map "\C-c\C-c" 'html-add-citation)
  (define-key html-mode-map "\C-c\C-e" 'html-add-emphasized)
  (define-key html-mode-map "\C-c\C-f" 'html-add-fixed)
  (define-key html-mode-map "\C-c\C-i" 'html-add-italic)
  (define-key html-mode-map "\C-c\C-k" 'html-add-keyboard)
  (define-key html-mode-map "\C-c\C-l" 'html-add-listing)
  (define-key html-mode-map "\C-c\C-m" 'html-add-sample)
  (define-key html-mode-map "\C-c\C-p" 'html-add-preformatted)
  (define-key html-mode-map "\C-c\C-s" 'html-add-strong)
  (define-key html-mode-map "\C-c\C-v" 'html-add-variable)
  (define-key html-mode-map "<" 'html-less-than)
  (define-key html-mode-map ">" 'html-greater-than)
  (define-key html-mode-map "&" 'html-ampersand)
  (define-key html-mode-map "\C-c<" 'html-real-less-than)
  (define-key html-mode-map "\C-c>" 'html-real-greater-than)
  (define-key html-mode-map "\C-c&" 'html-real-ampersand)
  (define-key html-mode-map "\C-c\C-rl" 'html-add-normal-link-to-region)
  (define-key html-mode-map "\C-c\C-rr" 'html-add-reference-to-region)
)

;;; --------------------------- buffer-local vars ----------------------------

(defvar html-link-counter-default 0)
(defvar html-link-counter nil)
(make-variable-buffer-local 'html-link-counter)
(setq-default html-link-counter html-link-counter-default)

;;; ------------------------------ highlighting ------------------------------

(if (and html-running-epoch html-use-highlighting)
    (progn
      (defvar html-deemphasize-style (make-style))
      (set-style-foreground html-deemphasize-style html-deemphasize-color)
      (defvar html-emphasize-style (make-style))
      (set-style-foreground html-emphasize-style html-emphasize-color)))

(if (and html-running-lemacs html-use-highlighting)
    (progn
      (defvar html-deemphasize-style (make-face 'html-deemphasize-face))
      (set-face-foreground html-deemphasize-style html-deemphasize-color)
      (defvar html-emphasize-style (make-face 'html-emphasize-face))
      (set-face-foreground html-emphasize-style html-emphasize-color)))

(if html-use-highlighting
    (progn
      (if html-running-lemacs
          (defun html-add-zone (start end style)
            "Add a Lucid Emacs extent from START to END with STYLE."
            (let ((extent (make-extent start end)))
              (set-extent-face extent style)
              (set-extent-data extent 'html-mode))))
      (if html-running-epoch
          (defun html-add-zone (start end style)
            "Add an Epoch zone from START to END with STYLE."
            (let ((zone (add-zone start end style)))
              (epoch::set-zone-data zone 'html-mode))))))

(defun html-maybe-deemphasize-region (start end)
  "Maybe deemphasize a region of text.  Region is from START to END."
  (and (or html-running-epoch html-running-lemacs)
       html-use-highlighting
       (html-add-zone start end html-deemphasize-style)))

;;; --------------------------------------------------------------------------
;;; ------------------------ command support routines ------------------------
;;; --------------------------------------------------------------------------

(defun html-add-link (link-object)
  "Add a link.  Single argument LINK-OBJECT is value of HREF in the
new anchor.  Mark is set after anchor."
  (let ((start (point)))
    (and html-give-anchors-numeric-names
         (setq html-link-counter (1+ html-link-counter)))
    (insert "<a")
    (and html-give-anchors-numeric-names
         (insert " name=" (format "%d" html-link-counter)))
    (insert " href=\"" link-object "\">")
    (html-maybe-deemphasize-region start (1- (point)))
    (insert "</a>")
    (push-mark)
    (forward-char -4)
    (html-maybe-deemphasize-region (1+ (point)) (+ (point) 4))))

(defun html-add-reference (ref-object)
  "Add a reference.  Single argument REF-OBJECT is value of NAME in the
new anchor.  Mark is set after anchor."
  (let ((start (point)))
    (insert "<a")
    (insert " name=\"" ref-object "\">")
    (html-maybe-deemphasize-region start (1- (point)))
    (insert "</a>")
    (push-mark)
    (forward-char -4)
    (html-maybe-deemphasize-region (1+ (point)) (+ (point) 4))))

(defun html-add-list-internal (type)
  "Set up a given type of list by opening the list start/end pair
and creating an initial element.  Single argument TYPE is a string,
assumed to be a valid HTML list type (e.g. \"UL\" or \"OL\").
Mark is set after list."
  (let ((start (point)))
    (insert "<" type ">\n")
    (html-maybe-deemphasize-region start (1- (point)))
    (insert "<li>")
    ;; Point goes right there.
    (save-excursion
      (insert "\n")
      (setq start (point))
      (insert "</" type ">\n")
      (html-maybe-deemphasize-region start (1- (point)))
      ;; Reuse start to set mark.
      (setq start (point)))
    (push-mark start t)))

(defun html-open-area (tag)
  "Open an area for entering text such as PRE, XMP, or LISTING."
  (let ((start (point)))
    (insert "<" tag ">\n")
    (html-maybe-deemphasize-region start (1- (point)))
    (save-excursion
      (insert "\n")
      (setq start (point))
      (insert "</" tag ">\n")
      (html-maybe-deemphasize-region start (1- (point)))
      ;; Reuse start to set mark.
      (setq start (point)))
    (push-mark start t)))

(defun html-open-field (tag)
  (let ((start (point)))
    (insert "<" tag ">")
    (html-maybe-deemphasize-region start (1- (point)))
    (setq start (point))
    (insert "</" tag ">")
    (html-maybe-deemphasize-region (1+ start) (point))
    (push-mark)
    (goto-char start)))

;;; --------------------------------------------------------------------------
;;; -------------------------------- commands --------------------------------
;;; --------------------------------------------------------------------------

;; C-c a
(defun html-add-address ()
  "Add an address."
  (interactive)
  (html-open-field "address"))

;; C-c b
(defun html-add-blockquote ()
  (interactive)
  (html-open-area "blockquote"))

;; C-c C-b
(defun html-add-bold ()
  (interactive)
  (html-open-field "b"))

;; C-c c
(defun html-add-code ()
  (interactive)
  (html-open-field "code"))

;; C-c C-c
(defun html-add-citation ()
  (interactive)
  (html-open-field "cite"))

;; C-c d
(defun html-add-description-list ()
  "Add a definition list.  Blah blah."
  (interactive)
  (let ((start (point)))
    (insert "<dl>\n")
    (html-maybe-deemphasize-region start (1- (point)))
    (insert "<dt>")
    ;; Point goes right there.
    (save-excursion
      (insert "\n<dd>\n")
      (setq start (point))
      (insert "</dl>\n")
      (html-maybe-deemphasize-region start (1- (point)))
      ;; Reuse start to set mark.
      (setq start (point)))
    (push-mark start t)))

;; C-c e
(defun html-add-description-entry ()
  "Add a definition entry.  Assume we're at the end of a previous
entry."
  (interactive)
  (let ((start (point)))
    (insert "\n<dt>")
    (save-excursion
      (insert "\n<dd>"))))

;; C-c C-e
(defun html-add-emphasized ()
  (interactive)
  (html-open-field "em"))

;; C-c C-f
(defun html-add-fixed ()
  (interactive)
  (html-open-field "tt"))

;; C-c g
(defun html-add-img (href)
  "Add an img."
  (interactive "sImage URL: ")
  (let ((start (point)))
    (insert "<img src=\"" href "\">")
    (html-maybe-deemphasize-region (1+ start) (1- (point)))))

;; C-c h
(defun html-add-header (size header)
  "Add a header."
  (interactive "sSize (1-6; 1 biggest): \nsHeader: ")
  (let ((start (point)))
    (insert "<h" size ">")
    (html-maybe-deemphasize-region start (1- (point)))
    (insert header)
    (setq start (point))
    (insert "</h" size ">\n")
    (html-maybe-deemphasize-region (1+ start) (1- (point)))))

;; C-c i
(defun html-add-list-or-menu-item ()
  "Add a list or menu item.  Assume we're at the end of the
last item."
  (interactive)
  (let ((start (point)))
    (insert "\n<li> ")))

;; C-c C-i
(defun html-add-italic ()
  (interactive)
  (html-open-field "i"))

;; C-c C-k
(defun html-add-keyboard ()
  (interactive)
  (html-open-field "kbd"))

;; C-c l
(defun html-add-normal-link (link)
  "Make a link.  There is no completion of any kind yet."
  (interactive "sLink to: ")
  (html-add-link link))

;; C-c C-l
(defun html-add-listing ()
  (interactive)
  (html-open-area "listing"))

;; C-c m
(defun html-add-menu ()
  "Add a menu."
  (interactive)
  (html-add-list-internal "menu"))

;; C-c C-m
(defun html-add-sample ()
  (interactive)
  (html-open-field "samp"))

;; C-c n
(defun html-add-numbered-list ()
  "Add a numbered list."
  (interactive)
  (html-add-list-internal "ol"))

;; C-c p
(defun html-add-paragraph-separator ()
  "Add a paragraph separator."
  (interactive)
  (let ((start (point)))
    (insert "<p>")
    (html-maybe-deemphasize-region (+ start 1) (point))))

;; C-c C-p
(defun html-add-preformatted ()
  (interactive)
  (html-open-area "pre"))

;; C-c r
(defun html-add-normal-reference (reference)
  "Add a reference (named anchor)."
  (interactive "sReference name: ")
  (html-add-reference reference))

;; C-c s
(defun html-add-list ()
  "Add a list."
  (interactive)
  (html-add-list-internal "ul"))

;; C-c C-s
(defun html-add-strong ()
  (interactive)
  (html-open-field "strong"))

;; C-c t
(defun html-add-title (title)
  "Add or modify a title."
  (interactive "sTitle: ")
  (save-excursion
    (goto-char (point-min))
    (if (and (looking-at "<title>")
             (save-excursion
               (forward-char 7)
               (re-search-forward "[^<]*" 
                                  (save-excursion (end-of-line) (point)) 
                                  t)))
        ;; Plop the new title in its place.
        (replace-match title t)
      (insert "<title>")
      (html-maybe-deemphasize-region (point-min) (1- (point)))
      (insert title)
      (insert "</title>")
      (html-maybe-deemphasize-region (- (point) 7) (point))
      (insert "\n"))))

;; C-c C-v
(defun html-add-variable ()
  (interactive)
  (html-open-field "var"))

;; C-c x
(defun html-add-plaintext ()
  "Add plaintext."
  (interactive)
  (html-open-area "xmp"))

;;; --------------------------------------------------------------------------
;;; ---------------------------- region commands -----------------------------
;;; --------------------------------------------------------------------------

;; C-c C-r l
(defun html-add-normal-link-to-region (link start end)
  "Make a link that applies to the current region.  Again,
no completion."
  (interactive "sLink to: \nr")
  (save-excursion
    (goto-char end)
    (save-excursion
      (goto-char start)
      (and html-give-anchors-numeric-names
           (setq html-link-counter (1+ html-link-counter)))
      (insert "<a")
      (and html-give-anchors-numeric-names
           (insert " name=" (format "%d" html-link-counter)))
      (insert " href=\"" link "\">")
      (html-maybe-deemphasize-region start (1- (point))))
    (insert "</a>")
    (html-maybe-deemphasize-region (- (point) 3) (point))))

;; C-c C-r r
(defun html-add-reference-to-region (name start end)
  "Add a reference point (a link with no reference of its own) to
the current region."
  (interactive "sName (or RET for numeric): \nr")
  (and (string= name "")
       (progn
         (setq html-link-counter (1+ html-link-counter))
         (setq name (format "%d" html-link-counter))))
  (save-excursion
    (goto-char end)
    (save-excursion
      (goto-char start)
      (insert "<a name=" name ">")
      (html-maybe-deemphasize-region start (1- (point))))
    (insert "</a>")
    (html-maybe-deemphasize-region (- (point) 3) (point))))

;;; --------------------------------------------------------------------------
;;; ---------------------------- special commands ----------------------------
;;; --------------------------------------------------------------------------

(defun html-less-than ()
  (interactive)
  (insert "&lt;"))

(defun html-greater-than ()
  (interactive)
  (insert "&gt;"))

(defun html-ampersand ()
  (interactive)
  (insert "&amp;"))

(defun html-real-less-than ()
  (interactive)
  (insert "<"))

(defun html-real-greater-than ()
  (interactive)
  (insert ">"))

(defun html-real-ampersand ()
  (interactive)
  (insert "&"))

;;; --------------------------------------------------------------------------
;;; --------------------------- Mosaic previewing ----------------------------
;;; --------------------------------------------------------------------------

;; OK, we work like this: We have a variable html-previewer-process.
;; When we start, it's nil.  First time html-preview-document is
;; called, we write the current document into a tmp file and call
;; Mosaic on it.  Second time html-preview-document is called, we
;; write the current document into a tmp file, write out a tmp config
;; file, and send Mosaic SIGUSR1.

;; This feature REQUIRES the Lisp command signal-process, which seems
;; to be a Lucid Emacs v19 feature.  It might be in GNU Emacs v19 too;
;; I dunno.

(defvar html-previewer-process nil
  "Variable used to track live viewer process.")

(defun html-write-buffer-to-tmp-file ()
  "Write the current buffer to a temp file and return the name
of the tmp file."
  (let ((filename (concat "/tmp/" (make-temp-name "html") ".html")))
    (write-region (point-min) (point-max) filename nil 'foo)
    filename))

(defun html-preview-document ()
  "Preview the current buffer's HTML document by spawning off a
previewing process (assumed to be Mosaic, basically) and controlling
it with signals as long as it's alive."
  (interactive)
  (let ((tmp-file (html-write-buffer-to-tmp-file)))
    ;; If html-previewer-process is nil, we start a process.
    ;; OR if the process status is not equal to 'run.
    (if (or (eq html-previewer-process nil)
            (not (eq (process-status html-previewer-process) 'run)))
        (progn
          (message "Starting previewer...")
          (setq html-previewer-process
                (if html-document-previewer-args
                    (start-process "html-previewer" "html-previewer"
                                   html-document-previewer 
                                   html-document-previewer-args 
                                   tmp-file)
                  (start-process "html-previewer" "html-previewer"
                                 html-document-previewer 
                                 tmp-file))))
      ;; We've got a running previewer; use it via SIGUSR1.
      (save-excursion
        (let ((config-file (format "/tmp/xmosaic.%d" 
                                   (process-id html-previewer-process))))
          (set-buffer (generate-new-buffer "*html-preview-tmp*"))
          (insert "goto\nfile:" tmp-file "\n")
          (write-region (point-min) (point-max)
                        config-file nil 'foo)
          ;; This is a v19 routine only.
          (signal-process (process-id html-previewer-process)
                          html-sigusr1-signal-value)
          (delete-file config-file)
          (delete-file tmp-file)
          (kill-buffer (current-buffer)))))))

;;; --------------------------------------------------------------------------
;;; --------------------------------------------------------------------------
;;; --------------------------------------------------------------------------

;;; ----------------------- html-reorder-numeric-names -----------------------

(defun html-replace-string-in-buffer (start end newstring)
  (save-excursion
    (goto-char start)
    (delete-char (1+ (- end start)))
    (insert newstring)))

(defun html-reorder-numeric-names (&optional reorder-non-numeric)
  "Reorder the NAME fields for links in the current buffer.  The
new ordering starts at 1 and increases monotonically through the buffer.
If optional arg REORDER-NON-NUMERIC is non-nil, then non-numeric NAME's
will also be numbered, else they won't.

Beware that doing this will possibly mess up references to specific
links within this document (e.g., HREF=\"#12\") or by other documents.
This command is mainly intended for use during the initial creation
stage of a document, especially when this creation involves cutting
and pasting from other documents (which it shouldn't, since this is
hypertext :-)."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq html-link-counter 0)
    (while (re-search-forward "<A[ \t\n]+NAME=" (point-max) t)
      (let* ((start (match-end 0))
             (end (save-excursion
                    (re-search-forward "[ \t\n>]" 
                                       (point-max) 
                                       t)
                    (match-beginning 0)))
             (subst (buffer-substring start end)))
        (and subst
             ;; Proceed only if we reorder non-numeric links or
             ;; this is in fact numeric (i.e. > 0).
             (or reorder-non-numeric (> (string-to-int subst) 0))
             (progn
               (setq html-link-counter (1+ html-link-counter))
               (html-replace-string-in-buffer start (1- end)
                (format "%d" html-link-counter))))))))

;;; --------------------------- html-quotify-hrefs ---------------------------

(defun html-quotify-hrefs ()
  "Insert quotes around all HREF attribute value literals.

This remedies the problem with old HTML files that can't be processed
by SGML parsers. That is, changes <A HREF=foo> to <A HREF=\"foo\">."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while 
        (re-search-forward
         "<[aA][ \t\n]+\\([nN][aA][mM][eE]=[a-zA-Z0-9]+[ \t\n]+\\)?[hH][rR][eE][fF]="
         (point-max)
         t)
      (cond
       ((null (looking-at "\""))
        (insert "\"")
        (re-search-forward "[ \t\n>]" (point-max) t)
        (forward-char -1)
        (insert "\""))))))

;;; ------------------------------- html-mode --------------------------------

(defun html-mode ()
  "Major mode for editing HTML hypertext documents.  Special commands:\\{html-mode-map}
Turning on html-mode calls the value of the variable html-mode-hook,
if that value is non-nil.

More extensive documentation is available in the file 'html-mode.el'.
The latest (possibly unstable) version of this file will always be available
on anonymous FTP server ftp.ncsa.uiuc.edu in /outgoing/marca."
  (interactive)
  (kill-all-local-variables)
  (use-local-map html-mode-map)
  (setq mode-name "HTML")
  (setq major-mode 'html-mode)
  (setq local-abbrev-table html-mode-abbrev-table)
  (set-syntax-table html-mode-syntax-table)
  (run-hooks 'html-mode-hook))

;;; ------------------------------- our hooks --------------------------------

(defun html-html-mode-hook ()
  "Hook called from html-mode-hook.  Set html-link-counter to 
the highest link value in the document (the next link created will
be one greater than that) to insure unique (numeric) link ID's.
Also run htlm-quotify-hrefs if html-quotify-hrefs-on-find is non-nil."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<A[ \t\n]+NAME=" (point-max) t)
      (let* ((start (match-end 0))
             (end (save-excursion
                    (re-search-forward "[ \t\n>]"
                                       (point-max)
                                       t)
                    (match-beginning 0)))
             (subst (buffer-substring start end)))
        (and subst
             ;; Safe to do compare, since string-to-int passed a non-number
             ;; returns 0.
             (> (string-to-int subst) html-link-counter)
             (setq html-link-counter (string-to-int subst))))))
  ;; Quotify existing HREF's if html-quotify-hrefs-on-find is non-nil.
  (and html-quotify-hrefs-on-find (html-quotify-hrefs)))

;;; ------------------------------- hook setup -------------------------------

;; Author: Daniel LaLiberte (liberte@cs.uiuc.edu).
(defun html-postpend-unique-hook (hook-var hook-function)
  "Postpend HOOK-VAR with HOOK-FUNCTION, if it is not already an element.
hook-var's value may be a single function or a list of functions."
  (if (boundp hook-var)
      (let ((value (symbol-value hook-var)))
        (if (and (listp value) (not (eq (car value) 'lambda)))
            (and (not (memq hook-function value))
                 (set hook-var (append value (list hook-function))))
          (and (not (eq hook-function value))
               (set hook-var (append value (list hook-function))))))
    (set hook-var (list hook-function))))

(html-postpend-unique-hook 'html-mode-hook 'html-html-mode-hook)

;;; -------------------------- lucid menubar setup ---------------------------

(if html-running-lemacs
    (progn
      (defvar html-menu
        '("HTML Mode"
          ["Open Address"         html-add-address      t]
          ["Open Blockquote"      html-add-blockquote   t]
          ["Open Header"          html-add-header       t]
          ["Open Hyperlink"       html-add-normal-link  t]
          ["Open Listing"         html-add-listing      t]
          ["Open Plaintext"       html-add-plaintext    t]
          ["Open Preformatted"    html-add-preformatted t]
          ["Open Reference"       html-add-normal-reference    t]
          ["Open Title"           html-add-title        t]
          "----"
          ["Open Bold"            html-add-bold         t]
          ["Open Citation"        html-add-citation     t]
          ["Open Code"            html-add-code         t]
          ["Open Emphasized"      html-add-emphasized   t]
          ["Open Fixed"           html-add-fixed        t]
          ["Open Keyboard"        html-add-keyboard     t]
          ["Open Sample"          html-add-sample       t]
          ["Open Strong"          html-add-strong       t]
          ["Open Variable"        html-add-variable     t]
          "----"
          ["Add Inlined Image"    html-add-img          t]
          ["End Paragraph"        html-add-paragraph-separator t]
          ["Preview Document"     html-preview-document t]
          "----"
          ("Definition List ..."
           ["Open Definition List"    html-add-description-list  t]
           ["Add Definition Entry"    html-add-description-entry t]
           )
          ("Other Lists ..."
           ["Open Unnumbered List"    html-add-list          t]
           ["Open Numbered List"      html-add-numbered-list t]
           ["Open Menu"               html-add-menu          t]
           "----"
           ["Add List Or Menu Item"   html-add-list-or-menu-item   t]
           )           
          ("Operations On Region ..."
           ["Add Hyperlink To Region" html-add-normal-link-to-region  t]
           ["Add Reference To Region" html-add-reference-to-region    t]
           )
          ("Reserved Characters ..."
           ["Less Than (<)"           html-real-less-than      t]
           ["Greater Than (>)"        html-real-greater-than   t]
           ["Ampersand (&)"           html-real-ampersand      t]
           )
          )
        )

      (defun html-menu (e)
        (interactive "e")
        (mouse-set-point e)
        (beginning-of-line)
        (popup-menu html-menu))
      (define-key html-mode-map 'button3 'html-menu)

      (defun html-install-menubar ()
        (if (and current-menubar (not (assoc "HTML" current-menubar)))
            (progn
              (set-buffer-menubar (copy-sequence current-menubar))
              (add-menu nil "HTML" (cdr html-menu)))))
      (html-postpend-unique-hook 'html-mode-hook 'html-install-menubar)

      (defconst html-font-lock-keywords
        (list
         '("<\\([^>]*\\)>" . font-lock-comment-face)
         '("[Hh][Rr][Ee][Ff]=\"\\([^\"]*\\)\"" 1 font-lock-string-face t))
        "Patterns to highlight in HTML buffers.")
      (defun html-fontify ()
        (make-local-variable 'font-lock-keywords) 
        (setq font-lock-keywords html-font-lock-keywords)
        (font-lock-mode 1)
        (message "Hey boss, we been through html-fontify."))
      (html-postpend-unique-hook 'html-mode-hook 'html-fontify)
      )
  )

;;; ------------------------------ final setup -------------------------------

(or (assoc "\\.html$" auto-mode-alist)
    (setq auto-mode-alist (cons '("\\.html$" . html-mode) auto-mode-alist)))

(provide 'html-mode)

