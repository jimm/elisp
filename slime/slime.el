;;; slime.el --- Superior Lisp Interaction Mode for Emacs
;;
;;;; License
;;     Copyright (C) 2003  Eric Marsden, Luke Gorrie, Helmut Eller
;;     Copyright (C) 2004,2005,2006  Luke Gorrie, Helmut Eller
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.


;;;; Commentary
;;
;; This file contains extensions for programming in Common Lisp. The
;; main features are:
;;
;;   A socket-based communication/RPC interface between Emacs and
;;   Lisp.
;;
;;   The `slime-mode' minor-mode complementing `lisp-mode'. This new
;;   mode includes many commands for interacting with the Common Lisp
;;   process.
;;
;;   Common Lisp REPL (Read-Eval-Print Loop) written in Emacs Lisp,
;;   similar to `ielm'.
;;
;;   Common Lisp debugger written in Emacs Lisp. The debugger pops up
;;   an Emacs buffer similar to the Emacs/Elisp debugger.
;;
;;   Trapping compiler messages and creating annotations in the source
;;   file on the appropriate forms.
;;
;; SLIME is compatible with GNU Emacs 20 and 21 and XEmacs 21. In
;; order to run SLIME requires a supporting Lisp server called
;; Swank. Swank is distributed with slime.el and will automatically be
;; started in a normal installation.


;;;; Dependencies and setup

(eval-and-compile
  (require 'cl)
  (unless (fboundp 'define-minor-mode)
    (require 'easy-mmode)
    (defalias 'define-minor-mode 'easy-mmode-define-minor-mode))
  (when (locate-library "hyperspec")
    (require 'hyperspec)))
(require 'comint)
(require 'timer)
(require 'pp)
(require 'hideshow)
(require 'font-lock)
(when (featurep 'xemacs)
  (require 'overlay))
(require 'easymenu)

(defvar slime-lisp-modes '(lisp-mode))

(defun slime-setup (&optional contribs)
  "Setup Emacs so that lisp-mode buffers always use SLIME.
CONTRIBS is a list of contrib packages to load."
  (when (member 'lisp-mode slime-lisp-modes)
    (add-hook 'lisp-mode-hook 'slime-lisp-mode-hook))
  (when contribs
    (add-to-list 'load-path (expand-file-name "contrib" slime-path))
    (dolist (c contribs)
      (require c)
      (let ((init (intern (format "%s-init" c))))
        (when (fboundp init)
          (funcall init))))))

(defun slime-lisp-mode-hook ()
  (slime-mode 1)
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function))

(eval-and-compile 
  (defvar slime-path
    (let ((path (or (locate-library "slime") load-file-name)))
      (and path (file-name-directory path)))
    "Directory containing the Slime package.
This is used to load the supporting Common Lisp library, Swank.
The default value is automatically computed from the location of the
Emacs Lisp package."))

(eval-and-compile
  (defun slime-changelog-date ()
    "Return the datestring of the latest entry in the ChangeLog file.
Return nil if the ChangeLog file cannot be found."
    (let ((changelog (concat slime-path "ChangeLog")))
      (if (file-exists-p changelog)
          (with-temp-buffer 
            (insert-file-contents-literally changelog nil 0 100)
            (goto-char (point-min))
            (symbol-name (read (current-buffer))))
        nil))))

(defvar slime-protocol-version nil)
(setq slime-protocol-version
      (eval-when-compile (slime-changelog-date)))


;;;; Customize groups
;;
;;;;; slime

(defgroup slime nil
  "Interaction with the Superior Lisp Environment."
  :prefix "slime-"
  :group 'applications)

;;;;; slime-ui

(defgroup slime-ui nil
  "Interaction with the Superior Lisp Environment."
  :prefix "slime-"
  :group 'slime)

(defcustom slime-truncate-lines t
  "Set `truncate-lines' in popup buffers.
This applies to buffers that present lines as rows of data, such as
debugger backtraces and apropos listings."
  :type 'boolean
  :group 'slime-ui)

(defcustom slime-extended-modeline t
  "If non-nil, display various information in the mode line of a
Lisp buffer. The information includes the current connection of
that buffer, the buffer package, and some state indication."
  :type 'boolean
  :group 'slime-ui)

(defcustom slime-kill-without-query-p nil
  "If non-nil, kill SLIME processes without query when quitting Emacs.
This applies to the *inferior-lisp* buffer and the network connections."
  :type 'boolean
  :group 'slime-ui)

;;;;; slime-lisp

(defgroup slime-lisp nil
  "Lisp server configuration."
  :prefix "slime-"
  :group 'slime)

(defcustom slime-backend "swank-loader.lisp"
  "The name of the Lisp file that loads the Swank server.
This name is interpreted relative to the directory containing
slime.el, but could also be set to an absolute filename."
  :type 'string
  :group 'slime-lisp)

(defcustom slime-connected-hook nil
  "List of functions to call when SLIME connects to Lisp."
  :type 'hook
  :group 'slime-lisp)

(defcustom slime-enable-evaluate-in-emacs nil
  "*If non-nil, the inferior Lisp can evaluate arbitrary forms in Emacs.
The default is nil, as this feature can be a security risk."
  :type '(boolean)
  :group 'slime-lisp)

(defcustom slime-lisp-host "127.0.0.1"
  "The default hostname (or IP address) to connect to."
  :type 'string
  :group 'slime-lisp)

(defcustom slime-port 4005
  "Port to use as the default for `slime-connect'."
  :type 'integer
  :group 'slime-lisp)

;;;;; slime-mode

(defgroup slime-mode nil
  "Settings for slime-mode Lisp source buffers."
  :prefix "slime-"
  :group 'slime)

(defcustom slime-find-definitions-function 'slime-find-definitions-rpc
  "Function to find definitions for a name.
The function is called with the definition name, a string, as its
argument."
  :type 'function
  :group 'slime-mode
  :options '(slime-find-definitions-rpc
             slime-etags-definitions
             (lambda (name)
               (append (slime-find-definitions-rpc name)
                       (slime-etags-definitions name)))
             (lambda (name)
               (or (slime-find-definitions-rpc name)
                   (and tags-table-list
                        (slime-etags-definitions name))))))

(defcustom slime-complete-symbol-function 'slime-simple-complete-symbol
  "*Function to perform symbol completion."
  :group 'slime-mode
  :type '(choice (const :tag "Simple" slime-simple-complete-symbol)
                 (const :tag "Compound" slime-complete-symbol*)
                 (const :tag "Fuzzy" slime-fuzzy-complete-symbol)))

(defcustom slime-when-complete-filename-expand nil
  "Use comint-replace-by-expanded-filename instead of
comint-dynamic-complete-as-filename to complete file names"
  :group 'slime-mode
  :type 'boolean)

(defcustom slime-space-information-p t
  "Have the SPC key offer arglist information."
  :type 'boolean
  :group 'slime-mode)

;;;;; slime-mode-faces

(defgroup slime-mode-faces nil
  "Faces in slime-mode source code buffers."
  :prefix "slime-"
  :group 'slime-mode)

(defun slime-underline-color (color)
  "Return a legal value for the :underline face attribute based on COLOR."
  ;; In XEmacs the :underline attribute can only be a boolean.
  ;; In GNU it can be the name of a colour.
  (if (featurep 'xemacs)
      (if color t nil)
    color))

(defface slime-error-face
  `((((class color) (background light))
     (:underline ,(slime-underline-color "red")))
    (((class color) (background dark))
     (:underline ,(slime-underline-color "red")))
    (t (:underline t)))
  "Face for errors from the compiler."
  :group 'slime-mode-faces)

(defface slime-warning-face
  `((((class color) (background light))
     (:underline ,(slime-underline-color "orange")))
    (((class color) (background dark))
     (:underline ,(slime-underline-color "coral")))
    (t (:underline t)))
  "Face for warnings from the compiler."
  :group 'slime-mode-faces)

(defface slime-style-warning-face
  `((((class color) (background light))
     (:underline ,(slime-underline-color "brown")))
    (((class color) (background dark))
     (:underline ,(slime-underline-color "gold")))
    (t (:underline t)))
  "Face for style-warnings from the compiler."
  :group 'slime-mode-faces)

(defface slime-note-face
  `((((class color) (background light))
     (:underline ,(slime-underline-color "brown4")))
    (((class color) (background dark))
     (:underline ,(slime-underline-color "light goldenrod")))
    (t (:underline t)))
  "Face for notes from the compiler."
  :group 'slime-mode-faces)

(defun slime-face-inheritance-possible-p ()
  "Return true if the :inherit face attribute is supported." 
  (assq :inherit custom-face-attributes))

(defface slime-highlight-face
  (if (slime-face-inheritance-possible-p)
      '((t (:inherit highlight :underline nil)))
    '((((class color) (background light))
       (:background "darkseagreen2"))
      (((class color) (background dark))
       (:background "darkolivegreen"))
      (t (:inverse-video t))))
  "Face for compiler notes while selected."
  :group 'slime-mode-faces)

;;;;; sldb

(defgroup slime-debugger nil
  "Backtrace options and fontification."
  :prefix "sldb-"
  :group 'slime)

(defmacro define-sldb-faces (&rest faces)
  "Define the set of SLDB faces.
Each face specifiation is (NAME DESCRIPTION &optional PROPERTIES).
NAME is a symbol; the face will be called sldb-NAME-face.
DESCRIPTION is a one-liner for the customization buffer.
PROPERTIES specifies any default face properties."
  `(progn ,@(loop for face in faces
                  collect `(define-sldb-face ,@face))))

(defmacro define-sldb-face (name description &optional default)
  (let ((facename (intern (format "sldb-%s-face" (symbol-name name)))))
    `(defface ,facename
       (list (list t ,default))
      ,(format "Face for %s." description)
      :group 'slime-debugger)))

(define-sldb-faces
  (topline        "the top line describing the error")
  (condition      "the condition class")
  (section        "the labels of major sections in the debugger buffer")
  (frame-label    "backtrace frame numbers")
  (restart-type   "restart names."
                  (if (slime-face-inheritance-possible-p)
                      '(:inherit font-lock-keyword-face)))
  (restart        "restart descriptions")
  (restart-number "restart numbers (correspond to keystrokes to invoke)"
                  '(:bold t))
  (frame-line     "function names and arguments in the backtrace")
  (restartable-frame-line
   "frames which are surely restartable")
  (non-restartable-frame-line
   "frames which are surely not restartable")
  (detailed-frame-line
   "function names and arguments in a detailed (expanded) frame")
  (local-name     "local variable names")
  (local-value    "local variable values")
  (catch-tag      "catch tags"))

;;;;; slime-repl

(defgroup slime-repl nil
  "The Read-Eval-Print Loop (*slime-repl* buffer)."
  :prefix "slime-repl-"
  :group 'slime)

(defcustom slime-repl-shortcut-dispatch-char ?\,
  "Character used to distinguish repl commands from lisp forms."
  :type '(character)
  :group 'slime-repl)

(defcustom slime-repl-only-save-lisp-buffers t
  "When T we only attempt to save lisp-mode file buffers. When
  NIL slime will attempt to save all buffers (as per
  save-some-buffers). This applies to all ASDF related repl
  shortcuts."
  :type '(boolean)
  :group 'slime-repl)

(defface slime-repl-prompt-face
  (if (slime-face-inheritance-possible-p)
      '((t (:inherit font-lock-keyword-face)))
    '((((class color) (background light)) (:foreground "Purple"))
      (((class color) (background dark)) (:foreground "Cyan"))
      (t (:weight bold))))
  "Face for the prompt in the SLIME REPL."
  :group 'slime-repl)

(defface slime-repl-output-face
  (if (slime-face-inheritance-possible-p)
      '((t (:inherit font-lock-string-face)))
    '((((class color) (background light)) (:foreground "RosyBrown"))
      (((class color) (background dark)) (:foreground "LightSalmon"))
      (t (:slant italic))))
  "Face for Lisp output in the SLIME REPL."
  :group 'slime-repl)

(defface slime-repl-input-face
  '((t (:bold t)))
  "Face for previous input in the SLIME REPL."
  :group 'slime-repl)

(defface slime-repl-result-face
  '((t ()))
  "Face for the result of an evaluation in the SLIME REPL."
  :group 'slime-repl)

(defcustom slime-repl-history-file "~/.slime-history.eld"
  "File to save the persistent REPL history to."
  :type 'string
  :group 'slime-repl)

(defcustom slime-repl-history-size 200
  "*Maximum number of lines for persistent REPL history."
  :type 'integer
  :group 'slime-repl)


;;;; Minor modes

;;;;; slime-mode

(define-minor-mode slime-mode
  "\\<slime-mode-map>\
SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode).

Commands to compile the current buffer's source file and visually
highlight any resulting compiler notes and warnings:
\\[slime-compile-and-load-file]	- Compile and load the current buffer's file.
\\[slime-compile-file]	- Compile (but not load) the current buffer's file.
\\[slime-compile-defun]	- Compile the top-level form at point.

Commands for visiting compiler notes:
\\[slime-next-note]	- Goto the next form with a compiler note.
\\[slime-previous-note]	- Goto the previous form with a compiler note.
\\[slime-remove-notes]	- Remove compiler-note annotations in buffer.

Finding definitions:
\\[slime-edit-definition]	- Edit the definition of the function called at point.
\\[slime-pop-find-definition-stack]	- Pop the definition stack to go back from a definition.

Documentation commands:
\\[slime-describe-symbol]	- Describe symbol.
\\[slime-apropos]	- Apropos search.
\\[slime-disassemble-symbol]	- Disassemble a function.

Evaluation commands:
\\[slime-eval-defun]	- Evaluate top-level from containing point.
\\[slime-eval-last-expression]	- Evaluate sexp before point.
\\[slime-pprint-eval-last-expression]	- Evaluate sexp before point, pretty-print result.

Full set of commands:
\\{slime-mode-map}"
  nil
  nil
  ;; Fake binding to coax `define-minor-mode' to create the keymap
  '((" " 'undefined)))


(make-variable-buffer-local
 (defvar slime-modeline-string nil
   "The string that should be displayed in the modeline if
`slime-extended-modeline' is true, and which indicates the
current connection, package and state of a Lisp buffer.
The string is periodically updated by an idle timer."))


;;; These are used to keep track of old values, so we can determine
;;; whether the mode line has changed, and should be updated.
(make-variable-buffer-local
 (defvar slime-modeline-package nil))
(make-variable-buffer-local
 (defvar slime-modeline-connection-name nil))
(make-variable-buffer-local
 (defvar slime-modeline-connection-state nil))

(defun slime-compute-modeline-package ()
  (when (memq major-mode slime-lisp-modes)
    ;; WHEN-LET is defined later.
    (let ((package (slime-current-package)))
      (when package
        (slime-pretty-package-name package)))))

(defun slime-pretty-package-name (name)
  "Return a pretty version of a package name NAME."
  (let ((name (cond ((string-match "^#?:\\(.*\\)$" name)    
                     (match-string 1 name))
                    ((string-match "^\"\\(.*\\)\"$" name) 
                     (match-string 1 name))
                    (t name))))
    (format "%s" (read name))))

(defun slime-compute-modeline-connection ()
  (let ((conn (slime-current-connection)))
    (if (or (null conn) (slime-stale-connection-p conn)) 
        nil
        (slime-connection-name conn))))

(defun slime-compute-modeline-connection-state ()
  (let ((new-state (slime-compute-connection-state (slime-current-connection))))
    (if (eq new-state :connected)
        nil ; normal case, so don't display anything in the mode line.
        (slime-connection-state-as-string new-state))))

(defun slime-compute-modeline-string (conn state pkg)
  (concat (when (or conn pkg)             "[")
          (when pkg                       (format "PKG:%s" pkg))
          (when (and (or conn state) pkg) ", ")
          (when conn                      (format "CON:%s" conn))
          (when state                     (format "{%s}" state))
          (when (or conn pkg)             "]")))

(defun slime-update-modeline-string ()
  (let ((old-pkg   slime-modeline-package)
        (old-conn  slime-modeline-connection-name)
        (old-state slime-modeline-connection-state)
        (new-pkg   (slime-compute-modeline-package))
        (new-conn  (slime-compute-modeline-connection))
        (new-state (slime-compute-modeline-connection-state)))
    (when (or (not (equal old-pkg   new-pkg))
              (not (equal old-conn  new-conn))
              (not (equal old-state new-state)))
      (setq slime-modeline-package new-pkg)
      (setq slime-modeline-connection-name new-conn)
      (setq slime-modeline-connection-state new-state)
      (setq slime-modeline-string
            (slime-compute-modeline-string new-conn new-state new-pkg))
      (force-mode-line-update t))))

(defun slime-shall-we-update-modeline-p ()
  (and slime-extended-modeline 
       (or slime-mode slime-popup-buffer-mode)))

(defun slime-update-modeline ()
  (when (slime-shall-we-update-modeline-p)
    (slime-update-modeline-string)))

(run-with-idle-timer 0.2 0.2 'slime-update-modeline)

;; Setup the mode-line to say when we're in slime-mode, which
;; connection is active, and which CL package we think the current
;; buffer belongs to.
(add-to-list 'minor-mode-alist
             '(slime-mode
               (" Slime" slime-modeline-string)))


;;;;; Key bindings

;; See `slime-define-key' below for keyword meanings.
(defvar slime-keys
  '(;; Compiler notes
    ("\M-p" slime-previous-note)
    ("\M-n" slime-next-note)
    ("\M-c" slime-remove-notes :prefixed t)
    ("\C-k" slime-compile-and-load-file :prefixed t)
    ("\M-k" slime-compile-file :prefixed t)
    ("\C-c" slime-compile-defun :prefixed t)
    ("\C-l" slime-load-file :prefixed t)
    ;; Editing/navigating
    ("\M-\C-i" slime-complete-symbol :inferior t)
    ("\C-i" slime-complete-symbol :prefixed t :inferior t)
    ("\M-." slime-edit-definition :inferior t :sldb t)
    ("\C-x4." slime-edit-definition-other-window :inferior t :sldb t)
    ("\C-x5." slime-edit-definition-other-frame :inferior t :sldb t)
    ("\M-," slime-pop-find-definition-stack :inferior t :sldb t)
    ;; Evaluating
    ("\C-x\C-e" slime-eval-last-expression :inferior t)
    ("\C-x\M-e" slime-eval-last-expression-display-output :inferior t)
    ("\C-p" slime-pprint-eval-last-expression :prefixed t :inferior t)
    ("\C-r" slime-eval-region :prefixed t :inferior t)
    ("\C-\M-x" slime-eval-defun)
    (":"    slime-interactive-eval :prefixed t :sldb t)
    ("\C-e" slime-interactive-eval :prefixed t :sldb t :inferior t)
    ("\C-y" slime-call-defun :prefixed t)
    ("E"    slime-edit-value :prefixed t :sldb t :inferior t)
    ("\C-z" slime-switch-to-output-buffer :prefixed t :sldb t)
    ("\C-b" slime-interrupt :prefixed t :inferior t :sldb t)
    ("\M-g" slime-quit :prefixed t :inferior t :sldb t)
    ;; Documentation
    (" " slime-space :inferior t)
    ("\C-f" slime-describe-function :prefixed t :inferior t :sldb t)
    ("\M-d" slime-disassemble-symbol :prefixed t :inferior t :sldb t)
    ("\C-t" slime-toggle-trace-fdefinition :prefixed t :sldb t)
    ("\C-u" slime-undefine-function :prefixed t)
    ("\C-m" slime-macroexpand-1 :prefixed t :inferior t)
    ("\M-m" slime-macroexpand-all :prefixed t :inferior t)
    ("\M-0" slime-restore-window-configuration :prefixed t :inferior t)
    ([(control meta ?\.)] slime-next-location :inferior t)
    ("~" slime-sync-package-and-default-directory :prefixed t :inferior t)
    ("\M-p" slime-repl-set-package :prefixed t :inferior t)
    ;; Cross reference
    ("<" slime-list-callers :prefixed t :inferior t :sldb t)
    (">" slime-list-callees :prefixed t :inferior t :sldb t)
    ;; "Other"
    ("\I"  slime-inspect :prefixed t :inferior t :sldb t)
    ("\C-]" slime-close-all-parens-in-sexp :prefixed t :inferior t :sldb t)
    ("\C-xt" slime-list-threads :prefixed t :inferior t :sldb t)
    ("\C-xc" slime-list-connections :prefixed t :inferior t :sldb t)
    ;; ;; Shadow unwanted bindings from inf-lisp
    ;; ("\C-a" slime-nop :prefixed t :inferior t :sldb t)
    ;; ("\C-v" slime-nop :prefixed t :inferior t :sldb t)
    ))

(defun slime-nop ()
  "The null command. Used to shadow currently-unused keybindings."
  (interactive)
  (call-interactively 'undefined))

(defvar slime-doc-map (make-sparse-keymap)
  "Keymap for documentation commands. Bound to a prefix key.")

(defvar slime-doc-bindings
  '((?a slime-apropos)
    (?z slime-apropos-all)
    (?p slime-apropos-package)
    (?d slime-describe-symbol)
    (?f slime-describe-function)
    (?h slime-hyperspec-lookup)
    (?~ common-lisp-hyperspec-format)))
  
(defvar slime-who-map (make-sparse-keymap)
  "Keymap for who-xref commands. Bound to a prefix key.")

(defvar slime-who-bindings
  '((?c slime-who-calls)
    (?w slime-calls-who)
    (?r slime-who-references)
    (?b slime-who-binds)
    (?s slime-who-sets)
    (?m slime-who-macroexpands)
    (?a slime-who-specializes)))

;; Maybe a good idea, maybe not..
(defvar slime-prefix-key "\C-c"
  "The prefix key to use in SLIME keybinding sequences.")

(defvar slime-prefix-map (make-sparse-keymap)
  "Keymap for commands prefixed with `slime-prefix-key'.")

(defun* slime-define-key (key command &key prefixed)
  "Define a keybinding of KEY for COMMAND.
If PREFIXED is non-nil, `slime-prefix-key' is prepended to KEY."
  (cond (prefixed (define-key slime-prefix-map key command))
        (t (define-key slime-mode-map key command))))

(defun slime-init-keymaps ()
  "(Re)initialize the keymaps for `slime-mode'."
  (interactive)
  (setq slime-prefix-map (make-sparse-keymap))
  (define-key slime-mode-map slime-prefix-key slime-prefix-map)
  (loop for (key command . keys) in slime-keys
        do (apply #'slime-define-key key command :allow-other-keys t keys))
  ;; Documentation
  (setq slime-doc-map (make-sparse-keymap))
  (slime-define-both-key-bindings slime-doc-map slime-doc-bindings)
  ;; C-c C-d is the prefix for the doc map.
  (slime-define-key "\C-d" slime-doc-map :prefixed t)
  ;; Who-xref
  (setq slime-who-map (make-sparse-keymap))
  (slime-define-both-key-bindings slime-who-map slime-who-bindings)
  ;; C-c C-w is the prefix for the who-xref map.
  (slime-define-key "\C-w" slime-who-map :prefixed t))

(defun slime-define-both-key-bindings (keymap bindings)
  (loop for (char command) in bindings do
        ;; We bind both unmodified and with control.
        (define-key keymap `[,char] command)
        (unless (equal char ?h)     ; But don't bind C-h
          (define-key keymap `[(control ,char)] command))))

(slime-init-keymaps)


;;;; Setup initial `slime-mode' hooks

(make-variable-buffer-local
 (defvar slime-pre-command-actions nil
   "List of functions to execute before the next Emacs command.
This list of flushed between commands."))

(defun slime-pre-command-hook ()
  "Execute all functions in `slime-pre-command-actions', then NIL it."
  (dolist (undo-fn slime-pre-command-actions)
    (ignore-errors (funcall undo-fn)))
  (setq slime-pre-command-actions nil))

(defun slime-post-command-hook ()
  (when (null pre-command-hook) ; sometimes this is lost
    (add-hook 'pre-command-hook 'slime-pre-command-hook)))

(defun slime-setup-command-hooks ()
  "Setup a buffer-local `pre-command-hook' to call `slime-pre-command-hook'."
  (slime-add-local-hook 'pre-command-hook 'slime-pre-command-hook)
  (slime-add-local-hook 'post-command-hook 'slime-post-command-hook))


;;;; Framework'ey bits
;;;
;;; This section contains some standard SLIME idioms: basic macros,
;;; ways of showing messages to the user, etc. All the code in this
;;; file should use these functions when applicable.
;;;
;;;;; Syntactic sugar

(defmacro* when-let ((var value) &rest body)
  "Evaluate VALUE, and if the result is non-nil bind it to VAR and
evaluate BODY.

\(fn (VAR VALUE) &rest BODY)"
  `(let ((,var ,value))
     (when ,var ,@body)))

(put 'when-let 'lisp-indent-function 1)

(defmacro destructure-case (value &rest patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
	(operands (gensym "rand-"))
	(tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (case ,operator
	 ,@(mapcar (lambda (clause)
                     (if (eq (car clause) t)
                         `(t ,@(cdr clause))
                       (destructuring-bind ((op &rest rands) &rest body) clause
                         `(,op (destructuring-bind ,rands ,operands
                                 . ,body)))))
		   patterns)
	 ,@(if (eq (caar (last patterns)) t)
	       '()
	     `((t (error "Elisp destructure-case failed: %S" ,tmp))))))))

(put 'destructure-case 'lisp-indent-function 1)

(defmacro slime-define-keys (keymap &rest key-command)
  "Define keys in KEYMAP. Each KEY-COMMAND is a list of (KEY COMMAND)."
  `(progn . ,(mapcar (lambda (k-c) `(define-key ,keymap . ,k-c))
		     key-command)))

(put 'slime-define-keys 'lisp-indent-function 1)

(defmacro* with-struct ((conc-name &rest slots) struct &body body)
  "Like with-slots but works only for structs.
\(fn (CONC-NAME &rest SLOTS) STRUCT &body BODY)"
  (flet ((reader (slot) (intern (concat (symbol-name conc-name)
					(symbol-name slot)))))
    (let ((struct-var (gensym "struct")))
      `(let ((,struct-var ,struct))
	 (symbol-macrolet
	     ,(mapcar (lambda (slot)
			(etypecase slot
			  (symbol `(,slot (,(reader slot) ,struct-var)))
			  (cons `(,(first slot) (,(reader (second slot)) 
						 ,struct-var)))))
		      slots)
	   . ,body)))))

(put 'with-struct 'lisp-indent-function 2)

;;;;; Very-commonly-used functions

(defvar slime-message-function 'message)

;; Interface
(defun slime-message (format &rest args)
  "Like `message' but with special support for multi-line messages.
Single-line messages use the echo area."
  (apply slime-message-function format args))

(when (or (featurep 'xemacs)
          (= emacs-major-version 20))
  (setq slime-message-function 'slime-format-display-message))

(defun slime-format-display-message (format &rest args)
  (slime-display-message (apply #'format format args) "*SLIME Note*"))

(defun slime-display-message (message buffer-name) 
  "Display MESSAGE in the echo area or in BUFFER-NAME.
Use the echo area if MESSAGE needs only a single line.  If the MESSAGE
requires more than one line display it in BUFFER-NAME and add a hook
to `slime-pre-command-actions' to remove the window before the next
command."
  (when (get-buffer-window buffer-name) (delete-windows-on buffer-name))
  (cond ((or (string-match "\n" message)
             (> (length message) (1- (frame-width))))
         (lexical-let ((buffer (get-buffer-create buffer-name)))
           (with-current-buffer buffer
             (erase-buffer)
             (insert message)
             (goto-char (point-min))
             (let ((win (slime-create-message-window)))
               (set-window-buffer win (current-buffer))
               (shrink-window-if-larger-than-buffer
                (display-buffer (current-buffer)))))
           (push (lambda () (delete-windows-on buffer) (bury-buffer buffer))
                 slime-pre-command-actions)))
        (t (message "%s" message))))

(defun slime-create-message-window ()
  "Create a window at the bottom of the frame, above the minibuffer."
  (let ((previous (previous-window (minibuffer-window))))
    (when (<= (window-height previous) (* 2 window-min-height))
      (save-selected-window 
        (select-window previous)
        (enlarge-window (- (1+ (* 2 window-min-height))
                           (window-height previous)))))
    (split-window previous)))

(defvar slime-background-message-function 'slime-display-oneliner)

;; Interface
(defun slime-background-message (format-string &rest format-args)
  "Display a message in passing.
This is like `slime-message', but less distracting because it
will never pop up a buffer or display multi-line messages.
It should be used for \"background\" messages such as argument lists."
  (apply slime-background-message-function format-string format-args))

(defun slime-display-oneliner (format-string &rest format-args)
  (let* ((msg (apply #'format format-string format-args)))
    (unless (minibuffer-window-active-p (minibuffer-window))
      (message  "%s" (slime-oneliner msg)))))

(defun slime-oneliner (string)
  "Return STRING truncated to fit in a single echo-area line."
  (substring string 0 (min (length string)
                           (or (position ?\n string) most-positive-fixnum)
                           (1- (frame-width)))))

;; Interface
(defun slime-set-truncate-lines ()
  "Apply `slime-truncate-lines' to the current buffer."
  (when slime-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

;; Interface
(defun slime-read-package-name (prompt &optional initial-value)
  "Read a package name from the minibuffer, prompting with PROMPT."
  (let ((completion-ignore-case t))
    (completing-read prompt (slime-bogus-completion-alist 
                             (slime-eval 
                              `(swank:list-all-package-names t)))
		     nil t initial-value)))

;; Interface
(defun slime-read-symbol-name (prompt &optional query)
  "Either read a symbol name or choose the one at point.
The user is prompted if a prefix argument is in effect, if there is no
symbol at point, or if QUERY is non-nil.

This function avoids mistaking the REPL prompt for a symbol."
  (cond ((or current-prefix-arg query (not (slime-symbol-name-at-point)))
         (slime-read-from-minibuffer prompt (slime-symbol-name-at-point)))
        (t (slime-symbol-name-at-point))))

;; Interface
(defmacro slime-propertize-region (props &rest body)
  "Execute BODY and add PROPS to all the text it inserts.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (let ((start (gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
	 (add-text-properties ,start (point) ,props)))))

(put 'slime-propertize-region 'lisp-indent-function 1)

;; Interface
(defsubst slime-insert-propertized (props &rest args)
  "Insert all ARGS and then add text-PROPS to the inserted text."
  (slime-propertize-region props (apply #'insert args)))

(defmacro slime-with-rigid-indentation (level &rest body)
  "Execute BODY and then rigidly indent its text insertions.
Assumes all insertions are made at point."
  (let ((start (gensym)) (l (gensym)))
    `(let ((,start (point)) (,l ,(or level '(current-column))))
       (prog1 (progn ,@body)
         (slime-indent-rigidly ,start (point) ,l)))))

(put 'slime-with-rigid-indentation 'lisp-indent-function 1)

(defun slime-indent-rigidly (start end column)
  ;; Similar to `indent-rigidly' but doesn't inherit text props.
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (while (and (<= start (point))
                (progn
                  (save-excursion (insert-char ?\  column))
                  (zerop (forward-line -1)))))))

(defun slime-insert-indented (&rest strings)
  "Insert all arguments rigidly indented."
  (slime-with-rigid-indentation nil
    (apply #'insert strings)))

(defun slime-curry (fun &rest args)
  `(lambda (&rest more) (apply ',fun (append ',args more))))

(defun slime-rcurry (fun &rest args)
  `(lambda (&rest more) (apply ',fun (append more ',args))))

;;;;; Snapshots of current Emacs state

;;; Window configurations do not save (and hence not restore)
;;; any narrowing that could be applied to a buffer.
;;;
;;; For this purpose, we introduce a superset of a window
;;; configuration that does include the necessary information to
;;; properly restore narrowing.
;;;
;;; We call this superset an Emacs Snapshot.

(defstruct (slime-narrowing-configuration
             (:conc-name slime-narrowing-configuration.))
  narrowedp beg end)

(defstruct (slime-emacs-snapshot (:conc-name slime-emacs-snapshot.))
  ;; We explicitly store the value of point even though it's implicitly
  ;; stored in the window-configuration because Emacs provides no
  ;; way to access the things stored in a window configuration.
  window-configuration narrowing-configuration point-marker)

(defun slime-current-narrowing-configuration (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (make-slime-narrowing-configuration :narrowedp (slime-buffer-narrowed-p)
                                        :beg (point-min-marker)
                                        :end (point-max-marker))))

(defun slime-set-narrowing-configuration (narrowing-cfg)
  (when (slime-narrowing-configuration.narrowedp narrowing-cfg)
    (narrow-to-region (slime-narrowing-configuration.beg narrowing-cfg)
                      (slime-narrowing-configuration.end narrowing-cfg))))

(defun slime-current-emacs-snapshot (&optional frame)
  "Returns a snapshot of the current state of FRAME, or the
currently active frame if FRAME is not given respectively."
  (with-current-buffer
      (if frame
          (window-buffer (frame-selected-window (selected-frame)))
          (current-buffer))
    (make-slime-emacs-snapshot
     :window-configuration    (current-window-configuration frame)
     :narrowing-configuration (slime-current-narrowing-configuration)
     :point-marker            (point-marker))))

(defun slime-set-emacs-snapshot (snapshot)
  "Restores the state of Emacs according to the information saved
in SNAPSHOT."
  (let ((window-cfg    (slime-emacs-snapshot.window-configuration snapshot))
        (narrowing-cfg (slime-emacs-snapshot.narrowing-configuration snapshot))
        (marker        (slime-emacs-snapshot.point-marker snapshot)))
    (set-window-configuration window-cfg) ; restores previously current buffer.
    (slime-set-narrowing-configuration narrowing-cfg)
    (goto-char (marker-position marker))))

(defun slime-current-emacs-snapshot-fingerprint (&optional frame)
  "Return a fingerprint of the current emacs snapshot.
Fingerprints are `equalp' if and only if they represent window
configurations that are very similar (same windows and buffers.)

Unlike real window-configuration objects, fingerprints are not
sensitive to the point moving and they can't be restored."
  (mapcar (lambda (window) (list window (window-buffer window)))
          (slime-frame-windows frame)))

(defun slime-frame-windows (&optional frame)
  "Return the list of windows in FRAME."
  (loop with last-window = (previous-window (frame-first-window frame))
        for window = (frame-first-window frame) then (next-window window)
        collect window
        until (eq window last-window)))

;;;;; Temporary popup buffers

(make-variable-buffer-local
 (defvar slime-popup-buffer-saved-emacs-snapshot nil
   "The snapshot of the current state in Emacs before the popup-buffer
was displayed, so that this state can be restored later on.
Buffer local in popup-buffers."))

(make-variable-buffer-local
 (defvar slime-popup-buffer-saved-fingerprint nil
   "The emacs snapshot \"fingerprint\" after displaying the buffer."))

;; Interface
(defmacro* slime-with-popup-buffer ((name &optional package
                                          connection emacs-snapshot)
                                    &body body)
  "Similar to `with-output-to-temp-buffer'.
Bind standard-output and initialize some buffer-local variables.
Restore window configuration when closed.

NAME is the name of the buffer to be created.
PACKAGE is the value `slime-buffer-package'.
CONNECTION is the value for `slime-buffer-connection'.
If nil, no explicit connection is associated with
the buffer.  If t, the current connection is taken.

If EMACS-SNAPSHOT is non-NIL, it's used to restore the previous
state of Emacs after closing the temporary buffer. Otherwise, the
current state will be saved and later restored."
  `(let* ((vars% (list ,(if (eq package t) '(slime-current-package) package)
                       ,(if (eq connection t) '(slime-connection) connection)
                       ;; Defer the decision for NILness until runtime.
                       (or ,emacs-snapshot (slime-current-emacs-snapshot))))
          (standard-output (slime-popup-buffer ,name vars%)))
     (with-current-buffer standard-output
       (prog1 (progn ,@body)
         (assert (eq (current-buffer) standard-output))
         (setq buffer-read-only t)
         (slime-init-popup-buffer vars%)))))

(put 'slime-with-popup-buffer 'lisp-indent-function 1)

(defun slime-popup-buffer (name buffer-vars)
  "Return a temporary buffer called NAME.
The buffer also uses the minor-mode `slime-popup-buffer-mode'.
Pressing `q' in the buffer will restore the window configuration
to the way it is when the buffer was created, i.e. when this
function was called."
  (when (and (get-buffer name) (kill-buffer (get-buffer name))))
  (with-current-buffer (get-buffer-create name)
    (set-syntax-table lisp-mode-syntax-table)
    (prog1 (pop-to-buffer (current-buffer))
      (slime-init-popup-buffer buffer-vars))))

(defun slime-init-popup-buffer (buffer-vars)
  (slime-popup-buffer-mode 1)
  (setq slime-popup-buffer-saved-fingerprint
        (slime-current-emacs-snapshot-fingerprint))
  (multiple-value-setq (slime-buffer-package 
                        slime-buffer-connection
                        slime-popup-buffer-saved-emacs-snapshot)
    buffer-vars))

(define-minor-mode slime-popup-buffer-mode 
  "Mode for displaying read only stuff"
  nil
  (" Slime-Tmp" slime-modeline-string)
  '(("q" . slime-popup-buffer-quit-function)
    ("\C-c\C-z" . slime-switch-to-output-buffer)
    ("\M-." . slime-edit-definition)))

(make-variable-buffer-local
 (defvar slime-popup-buffer-quit-function 'slime-popup-buffer-quit
   "The function that is used to quit a temporary popup buffer."))

(defun slime-popup-buffer-quit-function (&optional kill-buffer-p)
  "Wrapper to invoke the value of `slime-popup-buffer-quit-function'."
  (interactive)
  (funcall slime-popup-buffer-quit-function kill-buffer-p))

;; Interface
(defun slime-popup-buffer-quit (&optional kill-buffer-p)
  "Get rid of the current (temp) buffer without asking.
Restore the window configuration unless it was changed since we
last activated the buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (when (slime-popup-buffer-snapshot-unchanged-p)
      (slime-popup-buffer-restore-snapshot))
    (with-current-buffer buffer
      (setq slime-popup-buffer-saved-emacs-snapshot nil) ; buffer-local var!
      (cond (kill-buffer-p (kill-buffer nil))
            (t (bury-buffer))))))

(defun slime-popup-buffer-snapshot-unchanged-p ()
  (equalp (slime-current-emacs-snapshot-fingerprint)
          slime-popup-buffer-saved-fingerprint))

(defun slime-popup-buffer-restore-snapshot ()
  (let ((snapshot slime-popup-buffer-saved-emacs-snapshot))
    (assert snapshot) 
    (slime-set-emacs-snapshot snapshot)))

;;;;; Filename translation
;;;
;;; Filenames passed between Emacs and Lisp should be translated using
;;; these functions. This way users who run Emacs and Lisp on separate
;;; machines have a chance to integrate file operations somehow.

(defvar slime-to-lisp-filename-function #'identity)
(defvar slime-from-lisp-filename-function #'identity)

(defun slime-to-lisp-filename (filename)
  "Translate the string FILENAME to a Lisp filename."
  (funcall slime-to-lisp-filename-function filename))

(defun slime-from-lisp-filename (filename)
  "Translate the Lisp filename FILENAME to an Emacs filename."
  (funcall slime-from-lisp-filename-function filename))


;;;; Starting SLIME
;;;
;;; This section covers starting an inferior-lisp, compiling and
;;; starting the server, initiating a network connection.

;;;;; Entry points

;; We no longer load inf-lisp, but we use this variable for backward
;; compatibility.
(defvar inferior-lisp-program "lisp" 
  "*Program name for invoking an inferior Lisp with for Inferior Lisp mode.")

(defvar slime-lisp-implementations nil
  "*A list of known Lisp implementations.
The list should have the form: 
  ((NAME (PROGRAM PROGRAM-ARGS...) &key INIT CODING-SYSTEM ENV) ...)

NAME is a symbol for the implementation.
PROGRAM and PROGRAM-ARGS are strings used to start the Lisp process.
INIT is a function that should return a string to load and start
  Swank. The function will be called with the PORT-FILENAME and ENCODING as
  arguments.  INIT defaults to `slime-init-command'. 
CODING-SYSTEM a symbol for the coding system. The default is 
  slime-net-coding-system
ENV environment variables for the subprocess (see `process-environment').

Here's an example: 
 ((cmucl (\"/opt/cmucl/bin/lisp\" \"-quiet\") :init slime-init-command)
  (acl (\"acl7\") :coding-system emacs-mule))")

(defvar slime-default-lisp nil
  "*The name of the default Lisp implementation.
See `slime-lisp-implementations'")

;; dummy definitions for the compiler
(defvar slime-net-coding-system)
(defvar slime-net-processes)
(defvar slime-default-connection)

(defun slime (&optional command coding-system)
  "Start an inferior^_superior Lisp and connect to its Swank server."
  (interactive)
  (let ((inferior-lisp-program (or command inferior-lisp-program))
        (slime-net-coding-system (or coding-system slime-net-coding-system)))
    (slime-start* (cond ((and command (symbolp command))
                         (slime-lisp-options command))
                        (t (slime-read-interactive-args))))))

(defvar slime-inferior-lisp-program-history '()
  "History list of command strings.  Used by `slime'.")
                                                  
(defun slime-read-interactive-args ()
  "Return the list of args which should be passed to `slime-start'.

The rules for selecting the arguments are rather complicated:

- In the most common case, i.e. if there's no prefix-arg in
  effect and if `slime-lisp-implementations' is nil, use
  `inferior-lisp-program' as fallback.

- If the table `slime-lisp-implementations' is non-nil use the
  implementation with name `slime-default-lisp' or if that's nil
  the first entry in the table.

- If the prefix-arg is `-', prompt for one of the registered
  lisps.

- If the prefix-arg is positive, read the command to start the
  process."
  (let ((table slime-lisp-implementations))
    (cond ((not current-prefix-arg) (slime-lisp-options))
          ((eq current-prefix-arg '-)
           (let ((key (completing-read 
                       "Lisp name: " (mapcar (lambda (x) 
                                               (list (symbol-name (car x)))) 
                                             table)
                       nil t)))
             (slime-lookup-lisp-implementation table (intern key))))
          (t
           (destructuring-bind (program &rest program-args)
               (split-string (read-string 
                              "Run lisp: " inferior-lisp-program
                              'slime-inferior-lisp-program-history))
             (let ((coding-system 
                    (if (eq 16 (prefix-numeric-value current-prefix-arg))
                        (read-coding-system "set slime-coding-system: "
                                            slime-net-coding-system)
                      slime-net-coding-system)))
               (list :program program :program-args program-args
                     :coding-system coding-system)))))))

(defun slime-lisp-options (&optional name)
  (let ((table slime-lisp-implementations))
    (assert (or (not name) table))
    (cond (table (slime-lookup-lisp-implementation slime-lisp-implementations 
                                                   (or name slime-default-lisp
                                                       (car (car table)))))
          (t (destructuring-bind (program &rest args)
                 (split-string inferior-lisp-program)
               (list :program program :program-args args))))))

(defun slime-lookup-lisp-implementation (table name)
  (destructuring-bind (name (prog &rest args) &rest keys) (assoc name table)
    (list* :name name :program prog :program-args args keys)))

(defun* slime-start (&key (program inferior-lisp-program) program-args 
                          directory
                          (coding-system slime-net-coding-system)
                          (init 'slime-init-command)
                          name
                          (buffer "*inferior-lisp*")
                          init-function
                          env)
  (let ((args (list :program program :program-args program-args :buffer buffer 
                    :coding-system coding-system :init init :name name
                    :init-function init-function :env env)))
    (slime-check-coding-system coding-system)
    (when (slime-bytecode-stale-p)
      (slime-urge-bytecode-recompile))
    (let ((proc (slime-maybe-start-lisp program program-args env
                                        directory buffer)))
      (slime-inferior-connect proc args)
      (pop-to-buffer (process-buffer proc)))))

(defun slime-start* (options)
  (apply #'slime-start options))

(defun slime-connect (host port &optional coding-system)
  "Connect to a running Swank server."
  (interactive (list (read-from-minibuffer "Host: " slime-lisp-host)
                     (read-from-minibuffer "Port: " (format "%d" slime-port)
                                           nil t)))
  (when (and (interactive-p) slime-net-processes
             (y-or-n-p "Close old connections first? "))
    (slime-disconnect))
  (message "Connecting to Swank on port %S.." port)
  (let ((coding-system (or coding-system slime-net-coding-system)))
    (slime-check-coding-system coding-system)
    (message "Connecting to Swank on port %S.." port)
    (let* ((process (slime-net-connect host port coding-system))
           (slime-dispatching-connection process))
      (slime-setup-connection process))))

(defun slime-start-and-load (filename &optional package)
  "Start Slime, if needed, load the current file and set the package."
  (interactive (list (expand-file-name (buffer-file-name))
                     (slime-find-buffer-package)))
  (cond ((slime-connected-p)
         (slime-load-file-set-package filename package))
        (t
         (slime-start-and-init (slime-lisp-options)
                               (slime-curry #'slime-start-and-load 
                                            filename package)))))

(defun slime-start-and-init (options fun)
  (let* ((rest (plist-get options :init-function))
         (init (cond (rest `(lambda () (funcall ',rest) (funcall ',fun)))
                     (t fun))))
    (slime-start* (plist-put (copy-list options) :init-function init))))

(defun slime-load-file-set-package (filename package)
  (let ((filename (slime-to-lisp-filename filename)))
    (slime-eval-async `(swank:load-file ,filename)
                      (lexical-let ((package package))
                        (lambda (ignored)
                          (slime-repl-set-package package))))))

;;;;; Start inferior lisp
;;;
;;; Here is the protocol for starting SLIME:
;;;
;;;   0. Emacs recompiles/reloads slime.elc if it exists and is stale.
;;;   1. Emacs starts an inferior Lisp process.
;;;   2. Emacs tells Lisp (via stdio) to load and start Swank.
;;;   3. Lisp recompiles the Swank if needed.
;;;   4. Lisp starts the Swank server and writes its TCP port to a temp file.
;;;   5. Emacs reads the temp file to get the port and then connects.
;;;   6. Emacs prints a message of warm encouragement for the hacking ahead.
;;;
;;; Between steps 2-5 Emacs polls for the creation of the temp file so
;;; that it can make the connection. This polling may continue for a
;;; fair while if Swank needs recompilation.

(defvar slime-connect-retry-timer nil
  "Timer object while waiting for an inferior-lisp to start.")

;;; Recompiling bytecode:

(defun slime-bytecode-stale-p ()
  "Return true if slime.elc is older than slime.el."
  (when-let (libfile (locate-library "slime"))
    (let* ((basename (file-name-sans-extension libfile))
           (sourcefile (concat basename ".el"))
           (bytefile (concat basename ".elc")))
      (and (file-exists-p bytefile)
           (file-newer-than-file-p sourcefile bytefile)))))

(defun slime-recompile-bytecode ()
  "Recompile and reload slime.
Warning: don't use this in XEmacs, it seems to crash it!"
  (interactive)
  (let ((sourcefile (concat (file-name-sans-extension (locate-library "slime"))
                            ".el")))
    (byte-compile-file sourcefile t)))

(defun slime-urge-bytecode-recompile ()
  "Urge the user to recompile slime.elc.
Return true if we have been given permission to continue."
  (cond ((featurep 'xemacs)
         ;; My XEmacs crashes and burns if I recompile/reload an elisp
         ;; file from itself. So they have to do it themself.
         (or (y-or-n-p "slime.elc is older than source.  Continue? ")
             (signal 'quit nil)))
        ((y-or-n-p "slime.elc is older than source.  Recompile first? ")
         (slime-recompile-bytecode))
        (t)))

(defun slime-abort-connection ()
  "Abort connection the current connection attempt."
  (interactive)
  (cond (slime-connect-retry-timer
         (slime-cancel-connect-retry-timer)
         (message "Cancelled connection attempt."))
        (t (error "Not connecting"))))

;;; Starting the inferior Lisp and loading Swank:

(defun slime-maybe-start-lisp (program program-args env directory buffer)
  "Return a new or existing inferior lisp process."
  (cond ((not (comint-check-proc buffer))
         (slime-start-lisp program program-args env directory buffer))
        ((slime-reinitialize-inferior-lisp-p program program-args env buffer)
         (when-let (conn (find (get-buffer-process buffer) slime-net-processes 
                               :key #'slime-inferior-process))
           (slime-net-close conn))
         (get-buffer-process buffer))
        (t (slime-start-lisp program program-args env directory
                             (generate-new-buffer-name buffer)))))

(defun slime-reinitialize-inferior-lisp-p (program program-args env buffer)
  (let ((args (slime-inferior-lisp-args (get-buffer-process buffer))))
    (and (equal (plist-get args :program) program)
         (equal (plist-get args :program-args) program-args)
         (equal (plist-get args :env) env)
         (not (y-or-n-p "Create an additional *inferior-lisp*? ")))))

(defun slime-start-lisp (program program-args env directory buffer)
  "Does the same as `inferior-lisp' but less ugly.
Return the created process."
  (with-current-buffer (get-buffer-create buffer)
    (when directory
      (cd (expand-file-name directory)))
    (comint-mode)
    (let ((process-environment (append env process-environment)))
      (comint-exec (current-buffer) "inferior-lisp" program nil program-args))
    (lisp-mode-variables t)
    (let ((proc (get-buffer-process (current-buffer))))
      (slime-set-query-on-exit-flag proc)
      proc)))

(defun slime-inferior-connect (process args)
  "Start a Swank server in the inferior Lisp and connect."
  (slime-delete-swank-port-file 'quiet)
  (slime-start-swank-server process args)
  (slime-read-port-and-connect process nil))

(defvar slime-inferior-lisp-args nil
  "A buffer local variable in the inferior proccess.")

(defun slime-start-swank-server (process args)
  "Start a Swank server on the inferior lisp."
  (destructuring-bind (&key coding-system init &allow-other-keys) args
    (with-current-buffer (process-buffer process)
      (make-local-variable 'slime-inferior-lisp-args)
      (setq slime-inferior-lisp-args args)
      (let ((str (funcall init (slime-swank-port-file) coding-system)))
        (goto-char (process-mark process)) 
        (insert-before-markers str)
        (process-send-string process str)))))

(defun slime-inferior-lisp-args (process)
  (with-current-buffer (process-buffer process)
    slime-inferior-lisp-args))

;; XXX load-server & start-server used to be separated. maybe that was  better.
(defun slime-init-command (port-filename coding-system)
  "Return a string to initialize Lisp."
  (let ((loader (if (file-name-absolute-p slime-backend)
                    slime-backend
                  (concat slime-path slime-backend)))
        (encoding (slime-coding-system-cl-name coding-system)))
    ;; Return a single form to avoid problems with buffered input.
    (format "%S\n\n"
            `(progn
               (load ,(expand-file-name loader) :verbose t)
               (funcall (read-from-string "swank-loader:init"))
               (funcall (read-from-string "swank:start-server")
                        ,port-filename
                        :coding-system ,encoding)))))

(defun slime-swank-port-file ()
  "Filename where the SWANK server writes its TCP port number."
  (concat (file-name-as-directory (slime-temp-directory))
          (format "slime.%S" (emacs-pid))))

(defun slime-temp-directory ()
  (cond ((fboundp 'temp-directory) (temp-directory))
        ((boundp 'temporary-file-directory) temporary-file-directory)
        (t "/tmp/")))

(defun slime-delete-swank-port-file (&optional quiet)
  (condition-case data
      (delete-file (slime-swank-port-file))
    (error
     (ecase quiet
       ((nil) (signal (car data) (cdr data)))
       (quiet)
       (message (message "Unable to delete swank port file %S"
                         (slime-swank-port-file)))))))

(defun slime-read-port-and-connect (inferior-process retries)
  (slime-cancel-connect-retry-timer)
  (slime-attempt-connection inferior-process retries 1))

(defun slime-attempt-connection (process retries attempt)
  ;; A small one-state machine to attempt a connection with
  ;; timer-based retries.
  (let ((file (slime-swank-port-file))) 
    (unless (active-minibuffer-window)
      (message "Polling %S.. (Abort with `M-x slime-abort-connection'.)" file))
    (cond ((and (file-exists-p file)
                (> (nth 7 (file-attributes file)) 0)) ; file size
           (slime-cancel-connect-retry-timer)
           (let ((port (slime-read-swank-port))
                 (args (slime-inferior-lisp-args process)))
             (slime-delete-swank-port-file 'message)
             (let ((c (slime-connect slime-lisp-host port
                                     (plist-get args :coding-system))))
               (slime-set-inferior-process c process))))
          ((and retries (zerop retries))
           (slime-cancel-connect-retry-timer)
           (message "Failed to connect to Swank."))
          (t
           (when (and (file-exists-p file) 
                      (zerop (nth 7 (file-attributes file))))
             (message "(Zero length port file)")
             ;; the file may be in the filesystem but not yet written
             (unless retries (setq retries 3)))
           (unless slime-connect-retry-timer
             (setq slime-connect-retry-timer
                   (run-with-timer
                    0.3 0.3
                    #'slime-timer-call #'slime-attempt-connection 
                    process (and retries (1- retries)) 
                    (1+ attempt))))))))
    
(defun slime-timer-call (fun &rest args)
  "Call function FUN with ARGS, reporting all errors.

The default condition handler for timer functions (see
`timer-event-handler') ignores errors."
  (condition-case data
      (apply fun args)
    (error (debug nil (list "Error in timer" fun args data)))))

(defun slime-cancel-connect-retry-timer ()
  (when slime-connect-retry-timer
    (cancel-timer slime-connect-retry-timer)
    (setq slime-connect-retry-timer nil)))

(defun slime-read-swank-port ()
  "Read the Swank server port number from the `slime-swank-port-file'."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents (slime-swank-port-file))
      (goto-char (point-min))
      (let ((port (read (current-buffer))))
        (assert (integerp port))
        port))))

(defun slime-hide-inferior-lisp-buffer ()
  "Display the REPL buffer instead of the *inferior-lisp* buffer."
  (let* ((buffer (if (slime-process) 
                     (process-buffer (slime-process))))
         (window (if buffer (get-buffer-window buffer t)))
         (repl-buffer (slime-output-buffer t))
         (repl-window (get-buffer-window repl-buffer)))
    (when buffer
      (bury-buffer buffer))
    (cond (repl-window
           (when window
             (delete-window window)))
          (window
           (set-window-buffer window repl-buffer))
          (t
           (pop-to-buffer repl-buffer)
           (goto-char (point-max))))))

;;; Words of encouragement

(defun slime-user-first-name ()
  (let ((name (if (string= (user-full-name) "")
                  (user-login-name)
                (user-full-name))))
    (string-match "^[^ ]*" name)
    (capitalize (match-string 0 name))))

(defvar slime-words-of-encouragement
  `("Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    "Your hacking starts... NOW!"
    "May the source be with you!"
    "Take this REPL, brother, and may it serve you well."
    "Lemonodor-fame is but a hack away!"
    ,(format "%s, this could be the start of a beautiful program."
             (slime-user-first-name)))
  "Scientifically-proven optimal words of hackerish encouragement.")

(defun slime-random-words-of-encouragement ()
  "Return a string of hackerish encouragement."
  (eval (nth (random (length slime-words-of-encouragement))
             slime-words-of-encouragement)))


;;;; Networking
;;;
;;; This section covers the low-level networking: establishing
;;; connections and encoding/decoding protocol messages.
;;;
;;; Each SLIME protocol message beings with a 3-byte length header
;;; followed by an S-expression as text. The sexp must be readable
;;; both by Emacs and by Common Lisp, so if it contains any embedded
;;; code fragments they should be sent as strings.
;;;
;;; The set of meaningful protocol messages are not specified
;;; here. They are defined elsewhere by the event-dispatching
;;; functions in this file and in swank.lisp.

(defvar slime-net-processes nil
  "List of processes (sockets) connected to Lisps.")

(defvar slime-net-process-close-hooks '()
  "List of functions called when a slime network connection closes.
The functions are called with the process as their argument.")

(defun slime-secret ()
  "Finds the magic secret from the user's home directory.
Returns nil if the file doesn't exist or is empty; otherwise the first
line of the file."
  (condition-case err
      (with-temp-buffer
	(insert-file-contents "~/.slime-secret")
	(goto-char (point-min))
	(buffer-substring (point-min) (line-end-position)))
    (file-error nil)))

;;; Interface
(defun slime-net-connect (host port coding-system)
  "Establish a connection with a CL."
  (let* ((inhibit-quit nil)
         (proc (open-network-stream "SLIME Lisp" nil host port))
         (buffer (slime-make-net-buffer " *cl-connection*")))
    (push proc slime-net-processes)
    (set-process-buffer proc buffer)
    (set-process-filter proc 'slime-net-filter)
    (set-process-sentinel proc 'slime-net-sentinel)
    (slime-set-query-on-exit-flag proc)
    (when (fboundp 'set-process-coding-system)
      (slime-check-coding-system coding-system)
      (set-process-coding-system proc coding-system coding-system))
    (when-let (secret (slime-secret))
      (slime-net-send secret proc))
    proc))

(defun slime-make-net-buffer (name)
  "Make a buffer suitable for a network process."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (buffer-disable-undo))
    buffer))

(defun slime-set-query-on-exit-flag (process)
  "Set PROCESS's query-on-exit-flag to `slime-kill-without-query-p'."
  (when slime-kill-without-query-p
    ;; avoid byte-compiler warnings
    (let ((fun (if (fboundp 'set-process-query-on-exit-flag)
                   'set-process-query-on-exit-flag
                 'process-kill-without-query)))
      (funcall fun process nil))))

;;;;; Coding system madness

(defvar slime-net-valid-coding-systems
  '((iso-latin-1-unix nil "iso-latin-1-unix")
    (iso-8859-1-unix  nil "iso-latin-1-unix")
    (binary           nil "iso-latin-1-unix")
    (utf-8-unix       t   "utf-8-unix")
    (emacs-mule-unix  t   "emacs-mule-unix")
    (euc-jp-unix      t   "euc-jp-unix"))
  "A list of valid coding systems. 
Each element is of the form: (NAME MULTIBYTEP CL-NAME)")

(defun slime-find-coding-system (name)
  "Return the coding system for the symbol NAME.
The result is either an element in `slime-net-valid-coding-systems'
of nil."
  (let* ((probe (assq name slime-net-valid-coding-systems)))
    (if (and probe (if (fboundp 'check-coding-system)
                       (ignore-errors (check-coding-system (car probe)))
                     (eq (car probe) 'binary)))
        probe)))

(defvar slime-net-coding-system
  (find-if 'slime-find-coding-system 
           '(iso-latin-1-unix iso-8859-1-unix binary))
  "*Coding system used for network connections.
See also `slime-net-valid-coding-systems'.")
  
(defun slime-check-coding-system (coding-system)
  "Signal an error if CODING-SYSTEM isn't a valid coding system."
  (interactive)
  (let ((props (slime-find-coding-system coding-system)))
    (unless props
      (error "Invalid slime-net-coding-system: %s. %s"
             coding-system (mapcar #'car slime-net-valid-coding-systems)))
    (when (and (second props) (boundp 'default-enable-multibyte-characters))
      (assert default-enable-multibyte-characters))
    t))

(defcustom slime-repl-history-file-coding-system 
  (cond ((slime-find-coding-system 'utf-8-unix) 'utf-8-unix)
        (t slime-net-coding-system))
  "*The coding system for the history file."
  :type 'symbol
  :group 'slime-repl)

(defun slime-coding-system-mulibyte-p (coding-system)
  (second (slime-find-coding-system coding-system)))

(defun slime-coding-system-cl-name (coding-system)
  (third (slime-find-coding-system coding-system)))

;;; Interface
(defun slime-net-send (sexp proc)
  "Send a SEXP to Lisp over the socket PROC.
This is the lowest level of communication. The sexp will be READ and
EVAL'd by Lisp."
  (let* ((msg (concat (slime-prin1-to-string sexp) "\n"))
         (string (concat (slime-net-encode-length (length msg)) msg))
         (coding-system (cdr (process-coding-system proc))))
    (slime-log-event sexp)
    (cond ((slime-safe-encoding-p coding-system string)
           (process-send-string proc string))
          (t (error "Coding system %s not suitable for %S"
                    coding-system string)))))

(defun slime-safe-encoding-p (coding-system string)
  "Return true iff CODING-SYSTEM can safely encode STRING."
  (if (featurep 'xemacs)
      ;; FIXME: XEmacs encodes non-encodeable chars as ?~ automatically
      t
    (or (let ((candidates (find-coding-systems-string string))
              (base (coding-system-base coding-system)))
          (or (equal candidates '(undecided))
              (memq base candidates)))
        (and (not (multibyte-string-p string))
             (not (slime-coding-system-mulibyte-p coding-system))))))

(defun slime-net-close (process &optional debug)
  (setq slime-net-processes (remove process slime-net-processes))
  (when (eq process slime-default-connection)
    (setq slime-default-connection nil))
  (cond (debug         
         (set-process-sentinel process 'ignore)
         (set-process-filter process 'ignore)
         (delete-process process))
        (t
         (run-hook-with-args 'slime-net-process-close-hooks process)
         ;; killing the buffer also closes the socket
         (kill-buffer (process-buffer process)))))

(defun slime-net-sentinel (process message)
  (message "Lisp connection closed unexpectedly: %s" message)
  (slime-net-close process))

;;; Socket input is handled by `slime-net-filter', which decodes any
;;; complete messages and hands them off to the event dispatcher.

(defun slime-net-filter (process string)
  "Accept output from the socket and process all complete messages."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (slime-process-available-input process))

(defun slime-process-available-input (process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (slime-net-have-input-p)
      (let ((event (slime-net-read-or-lose process))
            (ok nil))
        (slime-log-event event)
        (unwind-protect
            (save-current-buffer
              (slime-dispatch-event event process)
              (setq ok t))
          (unless ok
            (slime-run-when-idle 'slime-process-available-input process)))))))

(defun slime-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (slime-net-decode-length))))

(defun slime-run-when-idle (function &rest args)
  "Call FUNCTION as soon as Emacs is idle."
  (apply #'run-at-time 
         (if (featurep 'xemacs) itimer-short-interval 0) 
         nil function args))

(defun slime-net-read-or-lose (process)
  (condition-case error
      (slime-net-read)
    (error
     (debug)
     (slime-net-close process t)
     (error "net-read error: %S" error))))

(defun slime-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (slime-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length)))
    (assert (plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (current-buffer)))
      (delete-region (point-min) end))))

(defun slime-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun slime-net-encode-length (n)
  "Encode an integer into a 24-bit hex string."
  (format "%06x" n))

(defun slime-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length 
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))


;;;; Connections
;;;
;;; "Connections" are the high-level Emacs<->Lisp networking concept.
;;;
;;; Emacs has a connection to each Lisp process that it's interacting
;;; with. Typically there would only be one, but a user can choose to
;;; connect to many Lisps simultaneously.
;;;
;;; A connection consists of a control socket, optionally an extra
;;; socket dedicated to receiving Lisp output (an optimization), and a
;;; set of connection-local state variables.
;;;
;;; The state variables are stored as buffer-local variables in the
;;; control socket's process-buffer and are used via accessor
;;; functions. These variables include things like the *FEATURES* list
;;; and Unix Pid of the Lisp process.
;;;
;;; One connection is "current" at any given time. This is:
;;;   `slime-dispatching-connection' if dynamically bound, or
;;;   `slime-buffer-connection' if this is set buffer-local, or
;;;   `slime-default-connection' otherwise. 
;;;
;;; When you're invoking commands in your source files you'll be using
;;; `slime-default-connection'. This connection can be interactively
;;; reassigned via the connection-list buffer.
;;;
;;; When a command creates a new buffer it will set
;;; `slime-buffer-connection' so that commands in the new buffer will
;;; use the connection that the buffer originated from. For example,
;;; the apropos command creates the *Apropos* buffer and any command
;;; in that buffer (e.g. `M-.') will go to the same Lisp that did the
;;; apropos search. REPL buffers are similarly tied to their
;;; respective connections.
;;;
;;; When Emacs is dispatching some network message that arrived from a
;;; connection it will dynamically bind `slime-dispatching-connection'
;;; so that the event will be processed in the context of that
;;; connection.
;;;
;;; This is mostly transparent. The user should be aware that he can
;;; set the default connection to pick which Lisp handles commands in
;;; Lisp-mode source buffers, and slime hackers should be aware that
;;; they can tie a buffer to a specific connection. The rest takes
;;; care of itself.

(defvar slime-dispatching-connection nil
  "Network process currently executing.
This is dynamically bound while handling messages from Lisp; it
overrides `slime-buffer-connection' and `slime-default-connection'.")

(make-variable-buffer-local
 (defvar slime-buffer-connection nil
   "Network connection to use in the current buffer.
This overrides `slime-default-connection'."))

(defvar slime-default-connection nil
  "Network connection to use by default.
Used for all Lisp communication, except when overridden by
`slime-dispatching-connection' or `slime-buffer-connection'.")

(defun slime-current-connection ()
  "Return the connection to use for Lisp interaction.
Return nil if there's no connection."
  (or slime-dispatching-connection
      slime-buffer-connection
      slime-default-connection))

(defun slime-connection ()
  "Return the connection to use for Lisp interaction.
Signal an error if there's no connection."
  (let ((conn (slime-current-connection)))
    (cond ((and (not conn) slime-net-processes)
           (or (slime-auto-select-connection)
               (error "No default connection selected.")))
          ((not conn)
           (or (slime-auto-connect)
               (error "Not connected.")))
          ((not (eq (process-status conn) 'open))
           (error "Connection closed."))
          (t conn))))

(defvar slime-auto-connect 'never)

(defun slime-auto-connect ()
  (cond ((or (eq slime-auto-connect 'always)
             (and (eq slime-auto-connect 'ask)
                  (y-or-n-p "No connection.  Start Slime? ")))
         (save-window-excursion
           (slime)
           (while (not (slime-current-connection))
             (sleep-for 1))
           (slime-connection)))
        (t nil)))

(defvar slime-auto-select-connection 'ask)

(defun slime-auto-select-connection ()
  (let* ((c0 (car slime-net-processes))
         (c (cond ((eq slime-auto-select-connection 'always) c0)
                  ((and (eq slime-auto-select-connection 'ask)
                        (y-or-n-p 
                         (format "No default connection selected.  %s %s? "
                                 "Switch to" (slime-connection-name c0))))
                   c0))))
    (when c
      (slime-select-connection c)
      (message "Switching to connection: %s" (slime-connection-name c))
      c)))

(defun slime-select-connection (process)
  "Make PROCESS the default connection."
  (setq slime-default-connection process))

(defun slime-cycle-connections ()
  "Change current slime connection, and make it buffer local."
  (interactive)
  (let* ((tail (or (cdr (member (slime-current-connection)
                                slime-net-processes))
                   slime-net-processes))
         (p (car tail)))
    (slime-select-connection p)
    (unless (eq major-mode 'slime-repl-mode)
      (setq slime-buffer-connection p))
    (message "Lisp: %s %s" (slime-connection-name p) (process-contact p))))

(defmacro* slime-with-connection-buffer ((&optional process) &rest body)
  "Execute BODY in the process-buffer of PROCESS.
If PROCESS is not specified, `slime-connection' is used.

\(fn (&optional PROCESS) &body BODY))"
  `(with-current-buffer
       (process-buffer (or ,process (slime-connection)
                           (error "No connection")))
     ,@body))

(put 'slime-with-connection-buffer 'lisp-indent-function 1)


(defun slime-compute-connection-state (conn)
  (cond ((null conn) :disconnected) 
        ((slime-stale-connection-p conn) :stale)
        ((slime-debugged-connection-p conn) :debugged)
        ((and (slime-use-sigint-for-interrupt conn) 
              (slime-busy-p conn)) :busy)
        ((eq slime-buffer-connection conn) :local)
        (t :connected)))

(defun slime-connection-state-as-string (state)
  (case state
    (:connected       "")
    (:disconnected    "not connected")
    (:busy            "busy..")
    (:debugged        "debugged..")
    (:stale           "stale")
    (:local           "local")
    ))

;;; Connection-local variables:

(defmacro slime-def-connection-var (varname &rest initial-value-and-doc)
  "Define a connection-local variable.
The value of the variable can be read by calling the function of the
same name (it must not be accessed directly). The accessor function is
setf-able.

The actual variable bindings are stored buffer-local in the
process-buffers of connections. The accessor function refers to
the binding for `slime-connection'."
  (let ((real-var (intern (format "%s:connlocal" varname))))
    `(progn
       ;; Variable
       (make-variable-buffer-local
        (defvar ,real-var ,@initial-value-and-doc))
       ;; Accessor
       (defun ,varname (&optional process)
         (slime-with-connection-buffer (process) ,real-var))
       ;; Setf
       (defsetf ,varname (&optional process) (store)
         `(slime-with-connection-buffer (,process)
            (setq (\, (quote (\, real-var))) (\, store))
            (\, store)))
       '(\, varname))))

(put 'slime-def-connection-var 'lisp-indent-function 2)

;; Let's indulge in some pretty colours.
(unless (featurep 'xemacs)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(slime-def-connection-var\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face)))))

(slime-def-connection-var slime-connection-number nil
  "Serial number of a connection.
Bound in the connection's process-buffer.")

(slime-def-connection-var slime-lisp-features '()
  "The symbol-names of Lisp's *FEATURES*.
This is automatically synchronized from Lisp.")

(slime-def-connection-var slime-lisp-modules '()
  "The strings of Lisp's *MODULES*.")

(slime-def-connection-var slime-lisp-package
    "COMMON-LISP-USER"
  "The current package name of the Superior lisp.
This is automatically synchronized from Lisp.")

(slime-def-connection-var slime-lisp-package-prompt-string
    "CL-USER"
  "The current package name of the Superior lisp.
This is automatically synchronized from Lisp.")

(slime-def-connection-var slime-pid nil
  "The process id of the Lisp process.")

(slime-def-connection-var slime-lisp-implementation-type nil
  "The implementation type of the Lisp process.")

(slime-def-connection-var slime-lisp-implementation-version nil
  "The implementation type of the Lisp process.")

(slime-def-connection-var slime-lisp-implementation-name nil
  "The short name for the Lisp implementation.")

(slime-def-connection-var slime-connection-name nil
  "The short name for connection.")

(slime-def-connection-var slime-inferior-process nil
  "The inferior process for the connection if any.")

(slime-def-connection-var slime-communication-style nil
  "The communication style.")

(slime-def-connection-var slime-machine-instance nil
  "The name of the (remote) machine running the Lisp process.")

;;;;; Connection setup

(defvar slime-connection-counter 0
  "The number of SLIME connections made. For generating serial numbers.")

;;; Interface
(defun slime-setup-connection (process)
  "Make a connection out of PROCESS."
  (let ((slime-dispatching-connection process))
    (slime-init-connection-state process)
    (slime-select-connection process)
    process))

(defun slime-init-connection-state (proc)
  "Initialize connection state in the process-buffer of PROC."
  ;; To make life simpler for the user: if this is the only open
  ;; connection then reset the connection counter.
  (when (equal slime-net-processes (list proc))
    (setq slime-connection-counter 0))
  (slime-with-connection-buffer ()
    (setq slime-buffer-connection proc))
  (setf (slime-connection-number proc) (incf slime-connection-counter))
  ;; We do the rest of our initialization asynchronously. The current
  ;; function may be called from a timer, and if we setup the REPL
  ;; from a timer then it mysteriously uses the wrong keymap for the
  ;; first command.
  (slime-eval-async '(swank:connection-info)
                    (slime-curry #'slime-set-connection-info proc)))

(defun slime-set-connection-info (connection info)
  "Initialize CONNECTION with INFO received from Lisp."
  (let ((slime-dispatching-connection connection))
    (destructuring-bind (&key pid style lisp-implementation machine
                              features package version modules
                              &allow-other-keys) info
      (slime-check-version version connection)
      (setf (slime-pid) pid
            (slime-communication-style) style
            (slime-lisp-features) features
            (slime-lisp-modules) modules)
      (destructuring-bind (&key name prompt) package
        (setf (slime-lisp-package) name
              (slime-lisp-package-prompt-string) prompt))
      (destructuring-bind (&key type name version) lisp-implementation
        (setf (slime-lisp-implementation-type) type
              (slime-lisp-implementation-version) version
              (slime-lisp-implementation-name) name
              (slime-connection-name) (slime-generate-connection-name name)))
      (destructuring-bind (&key instance type version) machine
        (setf (slime-machine-instance) instance)))
    (let ((args (when-let (p (slime-inferior-process))
                  (slime-inferior-lisp-args p))))
      (when-let (name (plist-get args ':name))
        (unless (string= (slime-lisp-implementation-name) name)
          (setf (slime-connection-name)
                (slime-generate-connection-name (symbol-name name)))))
      (slime-hide-inferior-lisp-buffer)
      (slime-init-output-buffer connection)
      (slime-load-contribs)
      (run-hooks 'slime-connected-hook)
      (when-let (fun (plist-get args ':init-function))
        (funcall fun)))
    (message "Connected. %s" (slime-random-words-of-encouragement))))

(defun slime-check-version (version conn)
  (or (equal version slime-protocol-version)
      (equal slime-protocol-version 'ignore)
      (y-or-n-p 
       (format "Versions differ: %s (slime) vs. %s (swank). Continue? "
               slime-protocol-version version))
      (slime-net-close conn)
      (top-level)))

(defun slime-generate-connection-name (lisp-name)
  (loop for i from 1
        for name = lisp-name then (format "%s<%d>" lisp-name i)
        while (find name slime-net-processes 
                    :key #'slime-connection-name :test #'equal)
        finally (return name)))

(defun slime-connection-close-hook (process)
  (when (eq process slime-default-connection)
    (when slime-net-processes
      (slime-select-connection (car slime-net-processes))
      (message "Default connection closed; switched to #%S (%S)"
               (slime-connection-number)
               (slime-connection-name)))))

(add-hook 'slime-net-process-close-hooks 'slime-connection-close-hook)

;;;;; Commands on connections

(defun slime-disconnect ()
  "Disconnect all connections."
  (interactive)
  (mapc #'slime-net-close slime-net-processes))

(defun slime-connection-port (connection)
  "Return the remote port number of CONNECTION."
  (if (featurep 'xemacs)
      (car (process-id connection))
    (cadr (process-contact connection))))

(defun slime-process (&optional connection)
  "Return the Lisp process for CONNECTION (default `slime-connection').
Can return nil if there's no process object for the connection."
  (let ((proc (slime-inferior-process connection)))
    (if (and proc 
             (memq (process-status proc) '(run stop)))
        proc)))

;; Non-macro version to keep the file byte-compilable. 
(defun slime-set-inferior-process (connection process)
  (setf (slime-inferior-process connection) process))

(defun slime-use-sigint-for-interrupt (&optional connection)
  (let ((c (or connection (slime-connection))))
    (ecase (slime-communication-style c)
      ((:fd-handler nil) t)
      ((:spawn :sigio) nil))))

(defvar slime-inhibit-pipelining t
  "*If true, don't send background requests if Lisp is already busy.")

(defun slime-background-activities-enabled-p ()
  (and (or slime-mode 
           (eq major-mode 'sldb-mode)
           (eq major-mode 'slime-repl-mode))
       (let ((con (slime-current-connection)))
         (and con
              (eq (process-status con) 'open)))
       (or (not (slime-busy-p))
           (not slime-inhibit-pipelining))))


;;;; Communication protocol

;;;;; Emacs Lisp programming interface
;;;
;;; The programming interface for writing Emacs commands is based on
;;; remote procedure calls (RPCs). The basic operation is to ask Lisp
;;; to apply a named Lisp function to some arguments, then to do
;;; something with the result.
;;;
;;; Requests can be either synchronous (blocking) or asynchronous
;;; (with the result passed to a callback/continuation function).  If
;;; an error occurs during the request then the debugger is entered
;;; before the result arrives -- for synchronous evaluations this
;;; requires a recursive edit.
;;;
;;; You should use asynchronous evaluations (`slime-eval-async') for
;;; most things. Reserve synchronous evaluations (`slime-eval') for
;;; the cases where blocking Emacs is really appropriate (like
;;; completion) and that shouldn't trigger errors (e.g. not evaluate
;;; user-entered code).
;;;
;;; We have the concept of the "current Lisp package". RPC requests
;;; always say what package the user is making them from and the Lisp
;;; side binds that package to *BUFFER-PACKAGE* to use as it sees
;;; fit. The current package is defined as the buffer-local value of
;;; `slime-buffer-package' if set, and otherwise the package named by
;;; the nearest IN-PACKAGE as found by text search (first backwards,
;;; then forwards).
;;;
;;; Similarly we have the concept of the current thread, i.e. which
;;; thread in the Lisp process should handle the request. The current
;;; thread is determined solely by the buffer-local value of
;;; `slime-current-thread'. This is usually bound to t meaning "no
;;; particular thread", but can also be used to nominate a specific
;;; thread. The REPL and the debugger both use this feature to deal
;;; with specific threads.

(make-variable-buffer-local
 (defvar slime-current-thread t
   "The id of the current thread on the Lisp side.  
t means the \"current\" thread;
:repl-thread the thread that executes REPL requests;
fixnum a specific thread."))

(make-variable-buffer-local
 (defvar slime-buffer-package nil
   "The Lisp package associated with the current buffer.
This is set only in buffers bound to specific packages."))

;;; `slime-rex' is the RPC primitive which is used to implement both
;;; `slime-eval' and `slime-eval-async'. You can use it directly if
;;; you need to, but the others are usually more convenient.

(defmacro* slime-rex ((&rest saved-vars)
                      (sexp &optional 
                            (package '(slime-current-package))
                            (thread 'slime-current-thread))
                      &rest continuations)
  "(slime-rex (VAR ...) (SEXP &optional PACKAGE THREAD) CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Lisp.

PACKAGE is evaluated and Lisp binds *BUFFER-PACKAGE* to this package.
The default value is (slime-current-package).

CLAUSES is a list of patterns with same syntax as
`destructure-case'.  The result of the evaluation of SEXP is
dispatched on CLAUSES.  The result is either a sexp of the
form (:ok VALUE) or (:abort).  CLAUSES is executed
asynchronously.

Note: don't use backquote syntax for SEXP, because Emacs20 cannot
deal with that."
  (let ((result (gensym)))
    `(lexical-let ,(loop for var in saved-vars
                         collect (etypecase var
                                   (symbol (list var var))
                                   (cons var)))
       (slime-dispatch-event 
        (list :emacs-rex ,sexp ,package ,thread
              (lambda (,result)
                (destructure-case ,result
                  ,@continuations)))))))

(put 'slime-rex 'lisp-indent-function 2)

;;; Interface
(defun slime-current-package ()
  "Return the Common Lisp package in the current context.
If `slime-buffer-package' has a value then return that, otherwise
search for and read an `in-package' form.

The REPL buffer is a special case: its package is `slime-lisp-package'."
  (cond ((eq major-mode 'slime-repl-mode)
         (slime-lisp-package))
        (slime-buffer-package)
        (t (save-restriction
             (widen)
             (slime-find-buffer-package)))))

(defvar slime-find-buffer-package-function 'slime-search-buffer-package
  "*Function to use for `slime-find-buffer-package'.  
The result should be the package-name (a string)
or nil if nothing suitable can be found.")

(defun slime-find-buffer-package ()
  "Figure out which Lisp package the current buffer is associated with."
  (funcall slime-find-buffer-package-function))

;; When modifing this code consider cases like:
;;  (in-package #.*foo*)
;;  (in-package #:cl)
;;  (in-package :cl)
;;  (in-package "CL")
;;  (in-package |CL|)
;;  (in-package #+ansi-cl :cl #-ansi-cl 'lisp)
(defun slime-search-buffer-package ()
  (let ((case-fold-search t)
        (regexp (concat "^(\\(cl:\\|common-lisp:\\)?in-package\\>[ \t']*"
                        "\\([^)]+\\)[ \t]*)")))
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 2)))))

;;; Synchronous requests are implemented in terms of asynchronous
;;; ones. We make an asynchronous request with a continuation function
;;; that `throw's its result up to a `catch' and then enter a loop of
;;; handling I/O until that happens.

(defvar slime-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

(defun slime-eval (sexp &optional package)
  "Evaluate EXPR on the superior Lisp and return the result."
  (when (null package) (setq package (slime-current-package)))
  (let* ((tag (gensym (format "slime-result-%d-" 
                              (1+ (slime-continuation-counter)))))
	 (slime-stack-eval-tags (cons tag slime-stack-eval-tags)))
    (apply
     #'funcall 
     (catch tag
       (slime-rex (tag sexp)
           (sexp package)
         ((:ok value)
          (unless (member tag slime-stack-eval-tags)
            (error "Reply to canceled synchronous eval request tag=%S sexp=%S"
                   tag sexp))
          (throw tag (list #'identity value)))
         ((:abort)
          (throw tag (list #'error "Synchronous Lisp Evaluation aborted"))))
       (let ((debug-on-quit t)
             (inhibit-quit nil)
             (conn (slime-connection)))
         (while t 
           (unless (eq (process-status conn) 'open)
             (error "Lisp connection closed unexpectedly"))
           (slime-accept-process-output nil 0.01)))))))

(defun slime-eval-async (sexp &optional cont package)
  "Evaluate EXPR on the superior Lisp and call CONT with the result."
  (slime-rex (cont (buffer (current-buffer)))
      (sexp (or package (slime-current-package)))
    ((:ok result)
     (when cont
       (set-buffer buffer)
       (funcall cont result)))
    ((:abort)
     (message "Evaluation aborted."))))

;;; These functions can be handy too:

(defun slime-connected-p ()
  "Return true if the Swank connection is open."
  (not (null slime-net-processes)))

(defun slime-check-connected ()
  "Signal an error if we are not connected to Lisp."
  (unless (slime-connected-p)
    (error "Not connected. Use `%s' to start a Lisp."
           (substitute-command-keys "\\[slime]"))))

(defun slime-stale-connection-p (conn)
  (not (memq conn slime-net-processes)))

(defun slime-debugged-connection-p (conn)
  (and (sldb-debugged-continuations conn) t))

(defun slime-busy-p (&optional conn)
  "True if Lisp has outstanding requests.
Debugged requests are ignored."
  (let ((debugged (sldb-debugged-continuations (or conn (slime-connection)))))
    (remove-if (lambda (id) 
                 (memq id debugged))
               (slime-rex-continuations)
               :key #'car)))


;; dummy defvar for compiler
(defvar slime-repl-read-mode)

(defun slime-reading-p ()
  "True if Lisp is currently reading input from the REPL."
  (with-current-buffer (slime-output-buffer)
    slime-repl-read-mode))

(defun slime-sync ()
  "Block until the most recent request has finished."
  (when (slime-rex-continuations)
    (let ((tag (caar (slime-rex-continuations))))
      (while (find tag (slime-rex-continuations) :key #'car)
        (slime-accept-process-output nil 0.1)))))

(defun slime-ping ()
  "Check that communication works."
  (interactive)
  (message "%s" (slime-eval "PONG")))
 
;;;;; Protocol event handler (the guts)
;;;
;;; This is the protocol in all its glory. The input to this function
;;; is a protocol event that either originates within Emacs or arrived
;;; over the network from Lisp.
;;;
;;; Each event is a list beginning with a keyword and followed by
;;; arguments. The keyword identifies the type of event. Events
;;; originating from Emacs have names starting with :emacs- and events
;;; from Lisp don't.

(slime-def-connection-var slime-rex-continuations '()
  "List of (ID . FUNCTION) continuations waiting for RPC results.")

(slime-def-connection-var slime-continuation-counter 0
  "Continuation serial number counter.")

(defvar slime-event-hooks)

(defun slime-dispatch-event (event &optional process)
  (let ((slime-dispatching-connection (or process (slime-connection))))
    (or (run-hook-with-args-until-success 'slime-event-hooks event)
        (destructure-case event
          ((:write-string output &optional target)
           (slime-write-string output target))
          ((:emacs-rex form package thread continuation)
           (when (and (slime-use-sigint-for-interrupt) (slime-busy-p))
             (slime-display-oneliner "; pipelined request... %S" form))
           (let ((id (incf (slime-continuation-counter))))
             (push (cons id continuation) (slime-rex-continuations))
             (slime-send `(:emacs-rex ,form ,package ,thread ,id))))
          ((:return value id)
           (let ((rec (assq id (slime-rex-continuations))))
             (cond (rec (setf (slime-rex-continuations)
                              (remove rec (slime-rex-continuations)))
                        (funcall (cdr rec) value))
                   (t
                    (error "Unexpected reply: %S %S" id value)))))
          ((:debug-activate thread level &optional select)
           (assert thread)
           (sldb-activate thread level select))
          ((:debug thread level condition restarts frames conts)
           (assert thread)
           (sldb-setup thread level condition restarts frames conts))
          ((:debug-return thread level stepping)
           (assert thread)
           (sldb-exit thread level stepping))
          ((:emacs-interrupt thread)
           (slime-send `(:emacs-interrupt ,thread)))
          ((:read-string thread tag)
           (assert thread)
           (slime-repl-read-string thread tag))
          ((:y-or-n-p thread tag question)
           (slime-y-or-n-p thread tag question))
          ((:read-aborted thread tag)
           (assert thread)
           (slime-repl-abort-read thread tag))
          ((:emacs-return-string thread tag string)
           (slime-send `(:emacs-return-string ,thread ,tag ,string)))
          ;;
          ((:new-package package prompt-string)
           (setf (slime-lisp-package) package)
           (setf (slime-lisp-package-prompt-string) prompt-string))
          ((:new-features features)
           (setf (slime-lisp-features) features))
          ((:indentation-update info)
           (slime-handle-indentation-update info))
          ((:open-dedicated-output-stream port)
           (slime-open-stream-to-lisp port))
          ((:eval-no-wait fun args)
           (apply (intern fun) args))
          ((:eval thread tag form-string)
           (slime-check-eval-in-emacs-enabled)
           (slime-eval-for-lisp thread tag form-string))
          ((:emacs-return thread tag value)
           (slime-send `(:emacs-return ,thread ,tag ,value)))
          ((:ed what)
           (slime-ed what))
          ((:inspect what)
           (slime-open-inspector what))
          ((:background-message message)
           (slime-background-message "%s" message))
          ((:debug-condition thread message)
           (assert thread)
           (message "%s" message))
          ((:ping thread tag)
           (slime-send `(:emacs-pong ,thread ,tag)))
          ((:reader-error packet condition)
           (slime-with-popup-buffer ("*Slime Error*")
             (princ (format "Invalid protocol message:\n%s\n\n%S"
                            condition packet))
             (goto-char (point-min)))
           (error "Invalid protocol message"))))))

(defun slime-send (sexp)
  "Send SEXP directly over the wire on the current connection."
  (slime-net-send sexp (slime-connection)))

(defun slime-reset ()
  "Clear all pending continuations."
  (interactive)
  (setf (slime-rex-continuations) '())
  (mapc #'kill-buffer (sldb-buffers)))

(defun slime-send-sigint ()
  (interactive)
  (signal-process (slime-pid) 'SIGINT))

;;;;; Event logging to *slime-events*
;;;
;;; The *slime-events* buffer logs all protocol messages for debugging
;;; purposes. Optionally you can enable outline-mode in that buffer,
;;; which is convenient but slows things down significantly.

(defvar slime-log-events t
  "*Log protocol events to the *slime-events* buffer.")

(defvar slime-outline-mode-in-events-buffer nil
  "*Non-nil means use outline-mode in *slime-events*.")

(defvar slime-event-buffer-name "*slime-events*"
  "The name of the slime event buffer.")

(defun slime-log-event (event)
  "Record the fact that EVENT occurred."
  (when slime-log-events
    (with-current-buffer (slime-events-buffer)
      ;; trim?
      (when (> (buffer-size) 100000)
        (goto-char (/ (buffer-size) 2))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (point)))
      (goto-char (point-max))
      (save-excursion
        (slime-pprint-event event (current-buffer)))
      (when (and (boundp 'outline-minor-mode)
                 outline-minor-mode)
        (hide-entry))
      (goto-char (point-max)))))

(defun slime-pprint-event (event buffer)
  "Pretty print EVENT in BUFFER with limited depth and width."
  (let ((print-length 20)
	(print-level 6)
	(pp-escape-newlines t))
    (pp event buffer)))

(defun slime-events-buffer ()
  (or (get-buffer slime-event-buffer-name)
      (let ((buffer (get-buffer-create slime-event-buffer-name)))
        (with-current-buffer buffer
          (buffer-disable-undo)
          (set (make-local-variable 'outline-regexp) "^(")
          (set (make-local-variable 'comment-start) ";")
          (set (make-local-variable 'comment-end) "")
          (when slime-outline-mode-in-events-buffer
            (outline-minor-mode)))
        buffer)))


;;;; Stream output

(slime-def-connection-var slime-connection-output-buffer nil
  "The buffer for the REPL.  May be nil or a dead buffer.")

(make-variable-buffer-local
 (defvar slime-output-start nil
   "Marker for the start of the output for the evaluation."))

(make-variable-buffer-local
 (defvar slime-output-end nil
   "Marker for end of output. New output is inserted at this mark."))

;; dummy definitions for the compiler
(defvar slime-repl-package-stack)
(defvar slime-repl-directory-stack)
(defvar slime-repl-input-start-mark)
(defvar slime-repl-prompt-start-mark)

(defun slime-output-buffer (&optional noprompt)
  "Return the output buffer, create it if necessary."
  (let ((buffer (slime-connection-output-buffer)))
    (or (if (buffer-live-p buffer) buffer)
        (setf (slime-connection-output-buffer)
              (let ((connection (slime-connection)))
                (with-current-buffer (slime-repl-buffer t connection)
                  (unless (eq major-mode 'slime-repl-mode) 
                    (slime-repl-mode))
                  (setq slime-buffer-connection connection)
                  (slime-reset-repl-markers)
                  (unless noprompt 
                    (slime-repl-insert-prompt))
                  (current-buffer)))))))

(defvar slime-repl-banner-function 'slime-repl-insert-banner)

(defun slime-repl-update-banner ()
  (funcall slime-repl-banner-function)
  (goto-char (point-max))
  (slime-mark-output-start)
  (slime-mark-input-start)
  (slime-repl-insert-prompt))

(defun slime-repl-insert-banner ()
  (when (zerop (buffer-size))
    (let ((welcome (concat "; SLIME " (or (slime-changelog-date)
                                          "- ChangeLog file not found"))))
      (insert welcome))))

(defun slime-init-output-buffer (connection)
  (with-current-buffer (slime-output-buffer t)
    (setq slime-buffer-connection connection
          slime-repl-directory-stack '()
          slime-repl-package-stack '())
    (slime-repl-update-banner)))

(defun slime-display-output-buffer ()
  "Display the output buffer and scroll to bottom."
  (with-current-buffer (slime-output-buffer)
    (goto-char (point-max))
    (unless (get-buffer-window (current-buffer) t)
      (display-buffer (current-buffer) t))
    (slime-repl-show-maximum-output)))

(defmacro slime-with-output-end-mark (&rest body)
  "Execute BODY at `slime-output-end'.  

If point is initially at `slime-output-end' and the buffer is visible
update window-point afterwards.  If point is initially not at
`slime-output-end, execute body inside a `save-excursion' block."
  `(let ((body.. (lambda () ,@body))
         (updatep.. (and (eobp) (pos-visible-in-window-p))))
     (cond ((= (point) slime-output-end)
            (let ((start.. (point)))
              (funcall body..)
              (set-marker slime-output-end (point))
              (when (= start.. slime-repl-input-start-mark) 
                (set-marker slime-repl-input-start-mark (point)))))
           (t 
            (save-excursion 
              (goto-char slime-output-end)
              (funcall body..))))
     (when updatep..
       (slime-repl-show-maximum-output))))

(defun slime-output-filter (process string)
  (with-current-buffer (process-buffer process)
    (when (and (plusp (length string))
               (eq (process-status slime-buffer-connection) 'open))
      (slime-write-string string))))

(defvar slime-open-stream-hooks)

(defun slime-open-stream-to-lisp (port)
  (let ((stream (open-network-stream "*lisp-output-stream*" 
                                     (slime-with-connection-buffer ()
                                       (current-buffer))
				     slime-lisp-host port)))
    (slime-set-query-on-exit-flag stream)
    (set-process-filter stream 'slime-output-filter)
    (let ((pcs (process-coding-system (slime-current-connection))))
      (set-process-coding-system stream (car pcs) (cdr pcs)))
    (when-let (secret (slime-secret))
      (slime-net-send secret stream))
    (run-hook-with-args 'slime-open-stream-hooks stream)
    stream))

(defun slime-io-speed-test (&optional profile)
  "A simple minded benchmark for stream performance.
If a prefix argument is given, instrument the slime package for
profiling before running the benchmark."
  (interactive "P")
  (eval-and-compile
    (require 'elp))
  (elp-reset-all)
  (elp-restore-all)
  (load "slime.el")
  ;;(byte-compile-file "slime-net.el" t)
  ;;(setq slime-log-events nil)
  (setq slime-enable-evaluate-in-emacs t)
  ;;(setq slime-repl-enable-presentations nil)
  (when profile
    (elp-instrument-package "slime-"))
  (kill-buffer (slime-output-buffer))
  (switch-to-buffer (slime-output-buffer))
  (delete-other-windows)
  (sit-for 0)
  (slime-repl-send-string "(swank:io-speed-test 4000 1)")
  (let ((proc (slime-inferior-process)))
    (when proc
      (display-buffer (process-buffer proc) t)
      (goto-char (point-max)))))

(defvar slime-write-string-function 'slime-repl-write-string)

(defun slime-write-string (string &optional target)
  "Insert STRING in the REPL buffer or some other TARGET.
If TARGET is nil, insert STRING as regular process
output.  If TARGET is :repl-result, insert STRING as the result of the
evaluation.  Other values of TARGET map to an Emacs marker via the 
hashtable `slime-output-target-to-marker'; output is inserted at this marker."
  (funcall slime-write-string-function string target))

(defun slime-repl-write-string (string &optional target)
  (case target
    ((nil) (slime-repl-emit string))
    (:repl-result (slime-repl-emit-result string))
    (t (slime-emit-string string target))))

(defvar slime-repl-popup-on-output nil
  "Display the output buffer when some output is written.
This is set to nil after displaying the buffer.")

(defmacro slime-save-marker (marker &rest body)
  (let ((pos (gensym "pos")))
  `(let ((,pos (marker-position ,marker)))
     (prog1 (progn . ,body)
       (set-marker ,marker ,pos)))))

(put 'slime-save-marker 'lisp-indent-function 1)

(defun slime-repl-emit (string)
  ;; insert the string STRING in the output buffer
  (with-current-buffer (slime-output-buffer)
    (save-excursion
      (goto-char slime-output-end)
      (slime-save-marker slime-output-start
        (slime-propertize-region '(face slime-repl-output-face 
                                        rear-nonsticky (face))
          (insert-before-markers string)
          (when (and (= (point) slime-repl-prompt-start-mark)
                     (not (bolp)))
            (insert-before-markers "\n")
            (set-marker slime-output-end (1- (point)))))))
    (when slime-repl-popup-on-output
      (setq slime-repl-popup-on-output nil)
      (display-buffer (current-buffer)))
    (slime-repl-show-maximum-output)))

(defun slime-repl-emit-result (string &optional bol)
  ;; insert STRING and mark it as evaluation result
  (with-current-buffer (slime-output-buffer)
    (save-excursion
      (slime-save-marker slime-output-start
        (slime-save-marker slime-output-end
          (goto-char slime-repl-input-start-mark)
          (when (and bol (not (bolp))) (insert-before-markers "\n"))
          (slime-propertize-region `(face slime-repl-result-face
                                          rear-nonsticky (face))
            (insert-before-markers string)))))
    (slime-repl-show-maximum-output)))

(defvar slime-last-output-target-id 0
  "The last integer we used as a TARGET id.")

(defvar slime-output-target-to-marker
  (make-hash-table)
  "Map from TARGET ids to Emacs markers.
The markers indicate where output should be inserted.")

(defun slime-output-target-marker (target)
  "Return the marker where output for TARGET should be inserted."
  (case target
    ((nil)
     (with-current-buffer (slime-output-buffer)
       slime-output-end))
    (:repl-result
     (with-current-buffer (slime-output-buffer)
       slime-repl-input-start-mark))
    (t
     (gethash target slime-output-target-to-marker))))

(defun slime-emit-string (string target)
  "Insert STRING at target TARGET.
See `slime-output-target-to-marker'."
  (let* ((marker (slime-output-target-marker target))
         (buffer (and marker (marker-buffer marker))))
    (when buffer
      (with-current-buffer buffer
        (save-excursion 
          ;; Insert STRING at MARKER, then move MARKER behind
          ;; the insertion.
          (goto-char marker)
          (insert-before-markers string)
          (set-marker marker (point)))))))

(defun slime-switch-to-output-buffer ()
  "Select the output buffer, when possible in an existing window.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear."
  (interactive)
  (slime-pop-to-buffer (slime-output-buffer))
  (goto-char (point-max)))


;;;; REPL
;;
;; The REPL uses some markers to separate input from output.  The
;; usual configuration is as follows:
;; 
;;    ... output ...    ... result ...    prompt> ... input ...
;;    ^            ^                      ^       ^           ^
;;    output-start output-end  prompt-start       input-start point-max
;;
;; input-start is a right inserting marker, because
;; we want it to stay behind when the user inserts text.
;;
;; We maintain the following invariant:
;;
;;  output-start <= output-end <= input-start.
;;
;; This invariant is important, because we must be prepared for
;; asynchronous output and asynchronous reads.  ("Asynchronous" means,
;; triggered by Lisp and not by Emacs.)
;;
;; All output is inserted at the output-end marker.  Some care must be
;; taken when output-end and input-start are at the same position: if
;; we insert at that point, we must move the right markers.  We should
;; also not leave (window-)point in the middle of the new output.  The
;; idiom we use is a combination to slime-save-marker,
;; insert-before-markers, and manually updating window-point
;; afterwards.
;;
;; A "synchronous" evaluation request proceeds as follows: the user
;; inserts some text between input-start and point-max and then hits
;; return.  We send that region to Lisp, move the output and input
;; makers to the line after the input and wait.  When we receive the
;; result, we insert it together with a prompt between the output-end
;; and input-start mark.  See `slime-repl-insert-prompt'.
;;
;; It is possible that some output for such an evaluation request
;; arrives after the result.  This output is inserted before the
;; result (and before the prompt). 
;;
;; If we are in "reading" state, e.g., during a call to Y-OR-N-P,
;; there is no prompt between output-end and input-start.
;;

;; Small helper.
(defun slime-make-variables-buffer-local (&rest variables)
  (mapcar #'make-variable-buffer-local variables))

(slime-make-variables-buffer-local
 (defvar slime-repl-package-stack nil
   "The stack of packages visited in this repl.")

 (defvar slime-repl-directory-stack nil
   "The stack of default directories associated with this repl.")

 (defvar slime-repl-prompt-start-mark)
 (defvar slime-repl-input-start-mark)
 (defvar slime-repl-old-input-counter 0
   "Counter used to generate unique `slime-repl-old-input' properties.
This property value must be unique to avoid having adjacent inputs be
joined together."))

(defun slime-reset-repl-markers ()
  (dolist (markname '(slime-output-start
                      slime-output-end
                      slime-repl-prompt-start-mark
                      slime-repl-input-start-mark))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point))))

;;;;; REPL mode setup

(defvar slime-repl-mode-map)

(setq slime-repl-mode-map (make-sparse-keymap))
(set-keymap-parent slime-repl-mode-map lisp-mode-map)

(dolist (spec slime-keys)
  (destructuring-bind (key command &key inferior prefixed 
                           &allow-other-keys) spec
    (when inferior
      (let ((key (if prefixed (concat slime-prefix-key key) key)))
        (define-key slime-repl-mode-map key command)))))

(slime-define-keys slime-repl-mode-map
  ("\C-m" 'slime-repl-return)
  ([return] 'slime-repl-return)
  ("\C-j" 'slime-repl-newline-and-indent)
  ("\C-\M-m" 'slime-repl-closing-return)
  ([(control return)] 'slime-repl-closing-return)
  ("\C-a" 'slime-repl-bol)
  ([home] 'slime-repl-bol)
  ("\M-p" 'slime-repl-previous-input)
  ((kbd "C-<up>") 'slime-repl-backward-input)
  ("\M-n" 'slime-repl-next-input)
  ((kbd "C-<down>") 'slime-repl-forward-input)
  ("\M-r" 'slime-repl-previous-matching-input)
  ("\M-s" 'slime-repl-next-matching-input)
  ("\C-c\C-c" 'slime-interrupt)
  ("\C-c\C-b" 'slime-interrupt)
  ("\C-c:"    'slime-interactive-eval)
  ("\C-c\C-e" 'slime-interactive-eval)
  ("\C-cE"     'slime-edit-value)
  ;("\t"   'slime-complete-symbol)
  ("\t"   'slime-indent-and-complete-symbol)
  (" "    'slime-space)
  ("\C-c\C-d" slime-doc-map)
  ("\C-c\C-w" slime-who-map)
  ("\C-\M-x" 'slime-eval-defun)
  ("\C-c\C-o" 'slime-repl-clear-output)
  ("\C-c\M-o" 'slime-repl-clear-buffer)
  ("\C-c\C-t" 'slime-toggle-trace-fdefinition)
  ("\C-c\C-u" 'slime-repl-kill-input)
  ("\C-c\C-n" 'slime-repl-next-prompt)
  ("\C-c\C-p" 'slime-repl-previous-prompt)
  ("\C-c\C-l" 'slime-load-file)
  ("\C-c\C-k" 'slime-compile-and-load-file)
  ("\C-c\C-z" 'slime-nop))

(defun slime-repl-mode () 
  "Major mode for interacting with a superior Lisp.
\\{slime-repl-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'slime-repl-mode)
  (use-local-map slime-repl-mode-map)
  (lisp-mode-variables t)
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function)
  (setq font-lock-defaults nil)
  (setq mode-name "REPL")
  (setq slime-current-thread :repl-thread)
  (set (make-local-variable 'scroll-conservatively) 20)
  (set (make-local-variable 'scroll-margin) 0)
  (when slime-repl-history-file
    (slime-repl-safe-load-history)
    (slime-add-local-hook 'kill-buffer-hook 
                          'slime-repl-safe-save-merged-history))
  (add-hook 'kill-emacs-hook 'slime-repl-save-all-histories)
  (slime-setup-command-hooks)
  ;; At the REPL, we define beginning-of-defun and end-of-defun to be
  ;; the start of the previous prompt or next prompt respectively.
  ;; Notice the interplay with SLIME-REPL-BEGINNING-OF-DEFUN.
  (set (make-local-variable 'beginning-of-defun-function) 
       'slime-repl-mode-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 
       'slime-repl-mode-end-of-defun)
  (run-hooks 'slime-repl-mode-hook))

(defun slime-repl-buffer (&optional create connection)
  "Get the REPL buffer for the current connection; optionally create."
  (funcall (if create #'get-buffer-create #'get-buffer)
           (format "*slime-repl %s*" (slime-connection-name connection))))

(defun slime-repl ()
  (interactive)
  (slime-switch-to-output-buffer))

(defun slime-repl-mode-beginning-of-defun ()
  (slime-repl-previous-prompt)
  t)

(defun slime-repl-mode-end-of-defun ()
  (slime-repl-next-prompt)
  t)

(defun slime-repl-send-string (string &optional command-string)
  (cond (slime-repl-read-mode
         (slime-repl-return-string string))
        (t (slime-repl-eval-string string))))

(defun slime-repl-eval-string (string)
  (slime-rex ()
      ((list 'swank:listener-eval string) (slime-lisp-package))
    ((:ok result)
     (slime-repl-insert-result result))
    ((:abort)
     (slime-repl-show-abort))))

(defun slime-repl-insert-result (result)
  (with-current-buffer (slime-output-buffer)
    (save-excursion
      (when result
        (destructure-case result
          ((:values &rest strings)
           (cond ((null strings)
                  (slime-repl-emit-result "; No value\n" t))
                 (t
                  (dolist (s strings)
                    (slime-repl-emit-result s t)))))))
      (slime-repl-insert-prompt))
    (slime-repl-show-maximum-output)))

(defun slime-repl-show-abort ()
  (with-current-buffer (slime-output-buffer)
    (save-excursion
      (slime-save-marker slime-output-start
        (slime-save-marker slime-output-end
          (goto-char slime-output-end)
          (insert-before-markers "; Evaluation aborted.\n")
          (slime-repl-insert-prompt))))
    (slime-repl-show-maximum-output)))

(defun slime-repl-insert-prompt ()
  "Insert the prompt (before markers!).
Set point after the prompt.  
Return the position of the prompt beginning."
  (goto-char slime-repl-input-start-mark)
  (slime-save-marker slime-output-start
    (slime-save-marker slime-output-end
      (unless (bolp) (insert-before-markers "\n"))
      (let ((prompt-start (point))
            (prompt (format "%s> " (slime-lisp-package-prompt-string))))
        (slime-propertize-region
            '(face slime-repl-prompt-face read-only t intangible t
                   slime-repl-prompt t
                   ;; emacs stuff
                   rear-nonsticky (slime-repl-prompt read-only face intangible)
                   ;; xemacs stuff
                   start-open t end-open t)
          (insert-before-markers prompt))
        (set-marker slime-repl-prompt-start-mark prompt-start)
        prompt-start))))

(defun slime-repl-show-maximum-output ()
  "Put the end of the buffer at the bottom of the window."
  (when (eobp)
    (let ((win (get-buffer-window (current-buffer))))
      (when win
        (with-selected-window win
          (set-window-point win (point-max)) 
          (recenter -1))))))

(defvar slime-repl-current-input-hooks)

(defun slime-repl-current-input (&optional until-point-p)
  "Return the current input as string.
The input is the region from after the last prompt to the end of
buffer."
  (or (run-hook-with-args-until-success 'slime-repl-current-input-hooks 
                                        until-point-p)
      (buffer-substring-no-properties
       slime-repl-input-start-mark 
       (if until-point-p 
           (point) 
         (point-max)))))

(defun slime-property-position (text-property &optional object)
  "Return the first position of TEXT-PROPERTY, or nil."
  (if (get-text-property 0 text-property object)
      0
    (next-single-property-change 0 text-property object)))
  
(defun slime-mark-input-start ()
  (set-marker slime-repl-input-start-mark (point) (current-buffer)))

(defun slime-mark-output-start ()
  (set-marker slime-output-start (point))
  (set-marker slime-output-end (point)))

(defun slime-mark-output-end ()
  ;; Don't put slime-repl-output-face again; it would remove the
  ;; special presentation face, for instance in the SBCL inspector.
  (add-text-properties slime-output-start slime-output-end
                       '(;;face slime-repl-output-face 
                         rear-nonsticky (face))))

(defun slime-repl-bol ()
  "Go to the beginning of line or the prompt."
  (interactive)
  (cond ((and (>= (point) slime-repl-input-start-mark)
              (slime-same-line-p (point) slime-repl-input-start-mark))
         (goto-char slime-repl-input-start-mark))
        (t (beginning-of-line 1)))
  (slime-preserve-zmacs-region))

(defun slime-preserve-zmacs-region ()
  "In XEmacs, ensure that the zmacs-region stays active after this command."
  (when (boundp 'zmacs-region-stays)
    (set 'zmacs-region-stays t)))

(defun slime-repl-in-input-area-p ()
   (<= slime-repl-input-start-mark (point)))

(defun slime-repl-at-prompt-start-p ()
  ;; This will not work on non-current prompts.
  (= (point) slime-repl-input-start-mark))

(defun slime-repl-beginning-of-defun ()
  "Move to beginning of defun."
  (interactive)
  ;; We call BEGINNING-OF-DEFUN if we're at the start of a prompt
  ;; already, to trigger SLIME-REPL-MODE-BEGINNING-OF-DEFUN by means
  ;; of the locally bound BEGINNING-OF-DEFUN-FUNCTION, in order to
  ;; jump to the start of the previous prompt.
  (if (and (not (slime-repl-at-prompt-start-p))
           (slime-repl-in-input-area-p))
      (goto-char slime-repl-input-start-mark)
    (beginning-of-defun))
  t)

;; FIXME: this looks very strange
(defun slime-repl-end-of-defun ()
  "Move to next of defun."
  (interactive)
  ;; C.f. SLIME-REPL-BEGINNING-OF-DEFUN.
  (if (and (not (= (point) (point-max))) 
           (slime-repl-in-input-area-p))
      (goto-char (point-max))
    (end-of-defun))
  t)

(defun slime-repl-previous-prompt ()
  "Move backward to the previous prompt."
  (interactive)
  (slime-repl-find-prompt t))

(defun slime-repl-next-prompt ()
  "Move forward to the next prompt."
  (interactive)
  (slime-repl-find-prompt))
 
(defun slime-repl-find-prompt (&optional backward)
  (let ((origin (point))
        (prop 'slime-repl-prompt))
    (while (progn 
             (slime-search-property-change prop backward)
             (not (or (slime-end-of-proprange-p prop) (bobp) (eobp)))))
    (unless (slime-end-of-proprange-p prop)
      (goto-char origin))))

(defun slime-search-property-change (prop &optional backward)
  (cond (backward 
         (goto-char (previous-single-char-property-change (point) prop)))
        (t 
         (goto-char (next-single-char-property-change (point) prop)))))

(defun slime-end-of-proprange-p (property)
  (and (get-char-property (max 1 (1- (point))) property)
       (not (get-char-property (point) property))))

(defvar slime-repl-return-hooks)

(defun slime-repl-return (&optional end-of-input)
  "Evaluate the current input string, or insert a newline.  
Send the current input ony if a whole expression has been entered,
i.e. the parenthesis are matched. 

With prefix argument send the input even if the parenthesis are not
balanced."
  (interactive "P")
  (slime-check-connected)
  (cond (end-of-input
         (slime-repl-send-input))
        (slime-repl-read-mode ; bad style?
         (slime-repl-send-input t))
        ((and (get-text-property (point) 'slime-repl-old-input)
              (< (point) slime-repl-input-start-mark))
         (slime-repl-grab-old-input end-of-input)
         (slime-repl-recenter-if-needed))
        ((run-hook-with-args-until-success 'slime-repl-return-hooks))
        ((slime-input-complete-p slime-repl-input-start-mark (point-max))
         (slime-repl-send-input t))
        (t 
         (slime-repl-newline-and-indent)
         (message "[input not complete]"))))

(defun slime-repl-recenter-if-needed ()
  "Make sure that (point) is visible."
  (unless (pos-visible-in-window-p (point-max))
    (save-excursion
      (goto-char (point-max))
      (recenter -1))))

(defun slime-repl-send-input (&optional newline)
  "Goto to the end of the input and send the current input.
If NEWLINE is true then add a newline at the end of the input."
  (unless (slime-repl-in-input-area-p)
    (error "No input at point."))
  (goto-char (point-max))
  (let ((end (point))) ; end of input, without the newline
    (slime-repl-add-to-input-history 
     (buffer-substring slime-repl-input-start-mark end))
    (when newline 
      (insert "\n")
      (slime-repl-show-maximum-output))
    (let ((inhibit-modification-hooks t))
      (add-text-properties slime-repl-input-start-mark 
                           (point)
                           `(slime-repl-old-input
                             ,(incf slime-repl-old-input-counter))))
    (let ((overlay (make-overlay slime-repl-input-start-mark end)))
      ;; These properties are on an overlay so that they won't be taken
      ;; by kill/yank.
      (overlay-put overlay 'read-only t)
      (overlay-put overlay 'face 'slime-repl-input-face)))
  (let ((input (slime-repl-current-input)))
    (goto-char (point-max))
    (slime-mark-input-start)
    (slime-mark-output-start)
    (slime-repl-send-string input)))

(defun slime-repl-grab-old-input (replace)
  "Resend the old REPL input at point.  
If replace is non-nil the current input is replaced with the old
input; otherwise the new input is appended.  The old input has the
text property `slime-repl-old-input'."
  (multiple-value-bind (beg end) (slime-property-bounds 'slime-repl-old-input)
    (let ((old-input (buffer-substring beg end)) ;;preserve
          ;;properties, they will be removed later
          (offset (- (point) beg)))
      ;; Append the old input or replace the current input
      (cond (replace (goto-char slime-repl-input-start-mark))
            (t (goto-char (point-max))
               (unless (eq (char-before) ?\ )
                 (insert " "))))
      (delete-region (point) (point-max))
      (save-excursion 
        (insert old-input)
        (when (equal (char-before) ?\n) 
          (delete-char -1)))
      (forward-char offset))))

(defun slime-property-bounds (prop)
  "Return two the positions of the previous and next changes to PROP.
PROP is the name of a text property."
  (assert (get-text-property (point) prop))
  (let ((end (next-single-char-property-change (point) prop)))
    (list (previous-single-char-property-change end prop) end)))

(defun slime-repl-closing-return ()
  "Evaluate the current input string after closing all open lists."
  (interactive)
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region slime-repl-input-start-mark (point))
    (while (ignore-errors (save-excursion (backward-up-list 1)) t)
      (insert ")")))
  (slime-repl-return))

(defun slime-repl-newline-and-indent ()
  "Insert a newline, then indent the next line.
Restrict the buffer from the prompt for indentation, to avoid being
confused by strange characters (like unmatched quotes) appearing
earlier in the buffer."
  (interactive)
  (save-restriction
    (narrow-to-region slime-repl-prompt-start-mark (point-max))
    (insert "\n")
    (lisp-indent-line)))

(defun slime-input-complete-p (start end)
  "Return t if the region from START to END contains a complete sexp."
  (save-excursion
    (goto-char start)
    (cond ((looking-at "\\s *['`#]?[(\"]")
           (ignore-errors
             (save-restriction
               (narrow-to-region start end)
               ;; Keep stepping over blanks and sexps until the end of
               ;; buffer is reached or an error occurs. Tolerate extra
               ;; close parens.
               (loop do (skip-chars-forward " \t\r\n)")
                     until (eobp)
                     do (forward-sexp))
               t)))
          (t t))))

(defun slime-repl-delete-current-input ()
  (delete-region slime-repl-input-start-mark (point-max)))

(defun slime-repl-kill-input ()
  "Kill all text from the prompt to point."
  (interactive)
  (cond ((< (marker-position slime-repl-input-start-mark) (point))
         (kill-region slime-repl-input-start-mark (point)))
        ((= (point) (marker-position slime-repl-input-start-mark))
         (slime-repl-delete-current-input))))

(defun slime-repl-replace-input (string)
  (slime-repl-delete-current-input)
  (insert-and-inherit string))

(defun slime-repl-input-line-beginning-position ()
  (save-excursion
    (goto-char slime-repl-input-start-mark)
    (line-beginning-position)))

(defvar slime-repl-clear-buffer-hook)

(defun slime-repl-clear-buffer ()
  "Delete the output generated by the Lisp process."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) slime-repl-prompt-start-mark)
    (delete-region slime-output-start slime-output-end)
    (goto-char slime-repl-input-start-mark)
    (recenter))
  (run-hooks 'slime-repl-clear-buffer-hook))

(defun slime-repl-clear-output ()
  "Delete the output inserted since the last input."
  (interactive)
  (let ((start (save-excursion 
                 (slime-repl-previous-prompt)
                 (ignore-errors (forward-sexp))
                 (forward-line)
                 (point)))
        (end (1- (slime-repl-input-line-beginning-position))))
    (when (< start end)
      (let ((inhibit-read-only t))
        (delete-region start end)
        (save-excursion
          (goto-char start)
          (insert ";;; output flushed"))))))

(defun slime-indent-and-complete-symbol ()
  "Indent the current line and perform symbol completion.
First indent the line. If indenting doesn't move point, complete
the symbol. If there's no symbol at the point, show the arglist
for the most recently enclosed macro or function."
  (interactive)
  (let ((pos (point)))
    (unless (get-text-property (line-beginning-position) 'slime-repl-prompt)
      (lisp-indent-line))
    (when (= pos (point))
      (cond ((save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
             (slime-complete-symbol))
            ((memq (char-before) '(?\t ?\ ))
             (slime-echo-arglist))))))


(defun slime-repl-set-package (package)
  "Set the package of the REPL buffer to PACKAGE."
  (interactive (list (let* ((p (slime-current-package))
                            (p (and p (slime-pretty-package-name p)))
                            (p (and (not (equal p (slime-lisp-package))) p)))
                       (slime-read-package-name "Package: " p))))
  (with-current-buffer (slime-output-buffer)
    (let ((unfinished-input (slime-repl-current-input)))
      (destructuring-bind (name prompt-string)
          (slime-repl-shortcut-eval `(swank:set-package ,package))
        (setf (slime-lisp-package) name)
        (setf (slime-lisp-package-prompt-string) prompt-string)
        (slime-repl-insert-prompt)
        (insert unfinished-input)))))


;;;;; History

(defcustom slime-repl-wrap-history nil
  "*T to wrap history around when the end is reached."
  :type 'boolean
  :group 'slime-repl)

(make-variable-buffer-local
 (defvar slime-repl-input-history '()
   "History list of strings read from the REPL buffer."))

(defun slime-repl-add-to-input-history (string)
  "Add STRING to the input history.
Empty strings and duplicates are ignored."
  (unless (or (equal string "")
              (equal string (car slime-repl-input-history)))
    (push string slime-repl-input-history)))

;; These two vars contain the state of the last history search.  We
;; only use them if `last-command' was 'slime-repl-history-replace,
;; otherwise we reinitialize them.

(defvar slime-repl-input-history-position -1
  "Newer items have smaller indices.")

(defvar slime-repl-history-pattern nil
  "The regexp most recently used for finding input history.")

(defun slime-repl-history-replace (direction &optional regexp)
  "Replace the current input with the next line in DIRECTION.
DIRECTION is 'forward' or 'backward' (in the history list).
If REGEXP is non-nil, only lines matching REGEXP are considered."
  (setq slime-repl-history-pattern regexp)
  (let* ((min-pos -1)
         (max-pos (length slime-repl-input-history))
         (pos0 (cond ((slime-repl-history-search-in-progress-p)
                      slime-repl-input-history-position)
                     (t min-pos)))
         (pos (slime-repl-position-in-history pos0 direction (or regexp "")))
         (msg nil))
    (cond ((and (< min-pos pos) (< pos max-pos))
           (slime-repl-replace-input (nth pos slime-repl-input-history))
           (setq msg (format "History item: %d" pos)))
          ((not slime-repl-wrap-history)
           (setq msg (cond ((= pos min-pos) "End of history")
                           ((= pos max-pos) "Beginning of history"))))
          (slime-repl-wrap-history
           (setq pos (if (= pos min-pos) max-pos min-pos))
           (setq msg "Wrapped history")))
    (when (or (<= pos min-pos) (<= max-pos pos))
      (when regexp
        (setq msg (concat msg "; no matching item"))))
    ;;(message "%s [%d %d %s]" msg start-pos pos regexp)
    (message "%s%s" msg (cond ((not regexp) "")
                              (t (format "; current regexp: %s" regexp))))
    (setq slime-repl-input-history-position pos)
    (setq this-command 'slime-repl-history-replace)))

(defun slime-repl-history-search-in-progress-p ()
  (eq last-command 'slime-repl-history-replace))

(defun slime-repl-terminate-history-search ()
  (setq last-command this-command))

(defun slime-repl-position-in-history (start-pos direction regexp)
  "Return the position of the history item matching regexp.
Return -1 resp. the length of the history if no item matches"
  ;; Loop through the history list looking for a matching line
  (let* ((step (ecase direction
                 (forward -1)
                 (backward 1)))
         (history slime-repl-input-history)
         (len (length history)))
    (loop for pos = (+ start-pos step) then (+ pos step)
          if (< pos 0) return -1
          if (<= len pos) return len
          if (string-match regexp (nth pos history)) return pos)))

(defun slime-repl-previous-input ()
  "Cycle backwards through input history.
If the `last-command' was a history navigation command use the
same search pattern for this command.
Otherwise use the current input as search pattern."
  (interactive)
  (slime-repl-history-replace 'backward (slime-repl-history-pattern t)))

(defun slime-repl-next-input ()
  "Cycle forwards through input history.
See `slime-repl-previous-input'."
  (interactive)
  (slime-repl-history-replace 'forward (slime-repl-history-pattern t)))

(defun slime-repl-forward-input ()
  "Cycle forwards through input history."
  (interactive)
  (slime-repl-history-replace 'forward (slime-repl-history-pattern)))

(defun slime-repl-backward-input ()
  "Cycle backwards through input history."
  (interactive)
  (slime-repl-history-replace 'backward (slime-repl-history-pattern)))

(defun slime-repl-previous-matching-input (regexp)
  (interactive "sPrevious element matching (regexp): ")
  (slime-repl-terminate-history-search)
  (slime-repl-history-replace 'backward regexp))

(defun slime-repl-next-matching-input (regexp)
  (interactive "sNext element matching (regexp): ")
  (slime-repl-terminate-history-search)
  (slime-repl-history-replace 'forward regexp))

(defun slime-repl-history-pattern (&optional use-current-input)
  "Return the regexp for the navigation commands."
  (cond ((slime-repl-history-search-in-progress-p)
         slime-repl-history-pattern)
        (use-current-input
         (let ((str (slime-repl-current-input)))
           (cond ((string-match "^[ \n]*$" str) nil)
                 (t (concat "^" (regexp-quote str))))))
        (t nil)))

(defun slime-repl-delete-from-input-history (string)
  "Delete STRING from the repl input history. 

When string is not provided then clear the current repl input and
use it as an input.  This is useful to get rid of unwanted repl
history entries while navigating the repl history."
  (interactive (list (slime-repl-current-input)))
  (let ((merged-history 
         (slime-repl-merge-histories slime-repl-input-history
                                     (slime-repl-read-history nil t))))
    (setq slime-repl-input-history
          (delete* string merged-history :test #'string=))
    (slime-repl-save-history))
  (slime-repl-delete-current-input))

;;;;; Persistent History 

(defun slime-repl-merge-histories (old-hist new-hist)
  "Merge entries from OLD-HIST and NEW-HIST."
  ;; Newer items in each list are at the beginning.
  (let* ((ht (make-hash-table :test #'equal))
         (test (lambda (entry)
                 (or (gethash entry ht)
                     (progn (setf (gethash entry ht) t)
                            nil)))))
    (append (remove-if test new-hist)
            (remove-if test old-hist))))

(defun slime-repl-load-history (&optional filename)
  "Set the current SLIME REPL history.
It can be read either from FILENAME or `slime-repl-history-file' or
from a user defined filename."
  (interactive (list (slime-repl-read-history-filename)))
  (let ((file (or filename slime-repl-history-file)))
    (setq slime-repl-input-history (slime-repl-read-history file t))))

(defun slime-repl-read-history (&optional filename noerrer)
  "Read and return the history from FILENAME.  
The default value for FILENAME is `slime-repl-history-file'.
If NOERROR is true return and the file doesn't exits return nil."
  (let ((file (or filename slime-repl-history-file)))
    (cond ((not (file-readable-p file)) '())
          (t (with-temp-buffer
               (insert-file-contents file)
               (read (current-buffer)))))))

(defun slime-repl-read-history-filename ()
  (read-file-name "Use SLIME REPL history from file: " 
                  slime-repl-history-file))

(defun slime-repl-save-merged-history (&optional filename)
  "Read the history file, merge the current REPL history and save it.
This tries to be smart in merging the history from the file and the
current history in that it tries to detect the unique entries using
`slime-repl-merge-histories'."
  (interactive (list (slime-repl-read-history-filename)))
  (let ((file (or filename slime-repl-history-file)))
    (with-temp-message "saving history..."
      (let ((hist (slime-repl-merge-histories (slime-repl-read-history file t)
                                              slime-repl-input-history)))
        (slime-repl-save-history file hist)))))

(defun slime-repl-save-history (&optional filename history)
  "Simply save the current SLIME REPL history to a file.
When SLIME is setup to always load the old history and one uses only
one instance of slime all the time, there is no need to merge the
files and this function is sufficient.

When the list is longer than `slime-repl-history-size' it will be
truncated.  That part is untested, though!"
  (interactive (list (slime-repl-read-history-filename)))
  (let ((file (or filename slime-repl-history-file))
        (hist (or history slime-repl-input-history)))
    (unless (file-writable-p file)
      (error (format "History file not writable: %s" file)))
    (let ((hist (subseq hist 0 (min (length hist) slime-repl-history-size))))
      ;;(message "saving %s to %s\n" hist file)
      (with-temp-file file
        (let ((cs slime-repl-history-file-coding-system)
              (print-length nil) (print-level nil))
          (setq buffer-file-coding-system cs)
          (insert (format ";; -*- coding: %s -*-\n" cs))
          (insert ";; History for SLIME REPL. Automatically written.\n"
                  ";; Edit only if you know what you're doing\n")
          (prin1 (mapcar #'substring-no-properties hist) (current-buffer)))))))

(defun slime-repl-save-all-histories ()
  "Save the history in each repl buffer."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (eq major-mode 'slime-repl-mode)
        (slime-repl-safe-save-merged-history)))))

(defun slime-repl-safe-save-merged-history ()
  (slime-repl-call-with-handler 
   #'slime-repl-save-merged-history
   "%S while saving the history. Continue? "))

(defun slime-repl-safe-load-history ()
  (slime-repl-call-with-handler 
   #'slime-repl-load-history
   "%S while loading the history. Continue? "))

(defun slime-repl-call-with-handler (fun query)
  "Call FUN in the context of an error handler.
The handler will use qeuery to ask the use if the error should be ingored."
  (condition-case err
      (funcall fun)
    (error 
     (if (y-or-n-p (format query (error-message-string err)))
         nil
       (signal (car err) (cdr err))))))


;;;;; REPL Read Mode

(define-key slime-repl-mode-map
  (string slime-repl-shortcut-dispatch-char) 'slime-handle-repl-shortcut)

(define-minor-mode slime-repl-read-mode 
  "Mode the read input from Emacs
\\{slime-repl-read-mode-map}"
  nil
  "[read]"
  '(("\C-m" . slime-repl-return)
    ([return] . slime-repl-return)
    ("\C-c\C-b" . slime-repl-read-break)
    ("\C-c\C-c" . slime-repl-read-break)))

(make-variable-buffer-local
 (defvar slime-read-string-threads nil))

(make-variable-buffer-local
 (defvar slime-read-string-tags nil))

(defun slime-repl-read-string (thread tag)
  (slime-switch-to-output-buffer)
  (push thread slime-read-string-threads)
  (push tag slime-read-string-tags)
  (goto-char (point-max))
  (slime-mark-output-end)
  (slime-mark-input-start)
  (slime-repl-read-mode 1))

(defun slime-y-or-n-p (thread tag question)
  (slime-dispatch-event `(:emacs-return ,thread ,tag ,(y-or-n-p question))))

(defun slime-repl-return-string (string)
  (slime-dispatch-event `(:emacs-return-string 
                          ,(pop slime-read-string-threads)
                          ,(pop slime-read-string-tags)
                          ,string))
  (slime-repl-read-mode -1))

(defun slime-repl-read-break ()
  (interactive)
  (slime-dispatch-event `(:emacs-interrupt ,(car slime-read-string-threads))))

(defun slime-repl-abort-read (thread tag)
  (with-current-buffer (slime-output-buffer)
    (pop slime-read-string-threads)
    (pop slime-read-string-tags)
    (slime-repl-read-mode -1)
    (message "Read aborted")))


;;;;; REPL handlers

(defstruct (slime-repl-shortcut (:conc-name slime-repl-shortcut.))
  symbol names handler one-liner)

(defvar slime-repl-shortcut-table nil
  "A list of slime-repl-shortcuts")

(defvar slime-repl-shortcut-history '()
  "History list of shortcut command names.")

(defvar slime-within-repl-shortcut-handler-p nil
  "Bound to T if we're in a REPL shortcut handler invoked from the REPL.")

(defun slime-handle-repl-shortcut ()
  (interactive)
  (if (> (point) slime-repl-input-start-mark)
      (insert (string slime-repl-shortcut-dispatch-char))
      (let ((shortcut (slime-lookup-shortcut
                       (completing-read "Command: " 
                                        (slime-bogus-completion-alist
                                         (slime-list-all-repl-shortcuts))
                                        nil t nil
                                        'slime-repl-shortcut-history))))
        (with-struct (slime-repl-shortcut. handler) shortcut
          (let ((slime-within-repl-shortcut-handler-p t))
            (call-interactively handler))))))

(defun slime-list-all-repl-shortcuts ()
  (loop for shortcut in slime-repl-shortcut-table
        append (slime-repl-shortcut.names shortcut)))

(defun slime-lookup-shortcut (name)
  (find-if (lambda (s) (member name (slime-repl-shortcut.names s)))
           slime-repl-shortcut-table))

(defmacro defslime-repl-shortcut (elisp-name names &rest options)
  "Define a new repl shortcut. ELISP-NAME is a symbol specifying
the name of the interactive function to create, or NIL if no
function should be created. 

NAMES is a list of \(full-name . aliases\). 

OPTIONS is an plist specifying the handler doing the actual work
of the shortcut \(`:handler'\), and a help text \(`:one-liner'\)."
  `(progn
     ,(when elisp-name
        `(defun ,elisp-name ()
           (interactive)
           (call-interactively ,(second (assoc :handler options)))))
     (let ((new-shortcut (make-slime-repl-shortcut
                          :symbol ',elisp-name
                          :names (list ,@names)
                          ,@(apply #'append options))))
       (setq slime-repl-shortcut-table
             (remove-if (lambda (s)
                          (member ',(car names) (slime-repl-shortcut.names s)))
                        slime-repl-shortcut-table))
       (push new-shortcut slime-repl-shortcut-table)
       ',elisp-name)))

(defun slime-repl-shortcut-eval (sexp &optional package)
  "This function should be used by REPL shortcut handlers instead
of `slime-eval' to evaluate their final expansion. (This
expansion will be added to the REPL's history.)"
  (when slime-within-repl-shortcut-handler-p ; were we invoked via ,foo?
    (slime-repl-add-to-input-history (prin1-to-string sexp)))
  (slime-eval sexp package))

(defun slime-repl-shortcut-eval-async (sexp &optional cont package)
  "This function should be used by REPL shortcut handlers instead
of `slime-eval-async' to evaluate their final expansion. (This
expansion will be added to the REPL's history.)"
  (when slime-within-repl-shortcut-handler-p ; were we invoked via ,foo?
    (slime-repl-add-to-input-history (prin1-to-string sexp)))
  (slime-eval-async sexp cont package))


(defun slime-list-repl-short-cuts ()
  (interactive)
  (slime-with-popup-buffer ("*slime-repl-help*")
    (let ((table (sort* (copy-list slime-repl-shortcut-table) #'string<
                        :key (lambda (x) 
                               (car (slime-repl-shortcut.names x))))))
      (dolist (shortcut table)
        (let ((names (slime-repl-shortcut.names shortcut)))
          (insert (pop names)) ;; first print the "full" name
          (when names
            ;; we also have aliases
            (insert " (aka ")
            (while (cdr names)
              (insert (pop names) ", "))
            (insert (car names) ")"))
        (insert "\n     " (slime-repl-shortcut.one-liner shortcut)
                "\n"))))))

(defun slime-save-some-lisp-buffers ()
  (if slime-repl-only-save-lisp-buffers
      (save-some-buffers nil (lambda ()
                               (and (memq major-mode slime-lisp-modes)
                                    (not (null buffer-file-name)))))
      (save-some-buffers)))
  

(defslime-repl-shortcut slime-repl-shortcut-help ("help" "?")
  (:handler 'slime-list-repl-short-cuts)
  (:one-liner "Display the help."))

(defslime-repl-shortcut nil ("change-directory" "!d" "cd")
  (:handler 'slime-set-default-directory)
  (:one-liner "Change the current directory."))

(defslime-repl-shortcut nil ("pwd")
  (:handler (lambda () 
              (interactive)
              (let ((dir (slime-eval `(swank:default-directory))))
                (message "Directory %s" dir))))
  (:one-liner "Show the current directory."))

(defslime-repl-shortcut slime-repl-push-directory
    ("push-directory" "+d" "pushd")
  (:handler (lambda (directory)
              (interactive
               (list (read-directory-name
                      "Push directory: "
                      (slime-eval '(swank:default-directory))
                      nil nil "")))
              (push (slime-eval '(swank:default-directory))
                    slime-repl-directory-stack)
              (slime-set-default-directory directory)))
  (:one-liner "Save the current directory and set it to a new one."))

(defslime-repl-shortcut slime-repl-pop-directory
    ("pop-directory" "-d" "popd")
  (:handler (lambda ()
              (interactive)
              (if (null slime-repl-directory-stack)
                  (message "Directory stack is empty.")
                  (slime-set-default-directory
                   (pop slime-repl-directory-stack)))))
  (:one-liner "Restore the last saved directory."))

(defslime-repl-shortcut nil ("change-package" "!p" "in-package" "in")
  (:handler 'slime-repl-set-package)
  (:one-liner "Change the current package."))

(defslime-repl-shortcut slime-repl-push-package ("push-package" "+p")
  (:handler (lambda (package)
              (interactive (list (slime-read-package-name "Package: ")))
              (push (slime-lisp-package) slime-repl-package-stack)
              (slime-repl-set-package package)))
  (:one-liner "Save the current package and set it to a new one."))

(defslime-repl-shortcut slime-repl-pop-package ("pop-package" "-p")
  (:handler (lambda ()
              (interactive)
              (if (null slime-repl-package-stack)
                  (message "Package stack is empty.")
                  (slime-repl-set-package
                   (pop slime-repl-package-stack)))))
  (:one-liner "Restore the last saved package."))

(defslime-repl-shortcut slime-repl-resend ("resend-form")
  (:handler (lambda ()
              (interactive)
              (insert (car slime-repl-input-history))
              (insert "\n")
              (slime-repl-send-input)))
  (:one-liner "Resend the last form."))

(defslime-repl-shortcut slime-repl-disconnect ("disconnect")
  (:handler 'slime-disconnect)
  (:one-liner "Disconnect all connections."))

(defslime-repl-shortcut slime-repl-sayoonara ("sayoonara")
  (:handler (lambda ()
              (interactive)
              (when (slime-connected-p)
                (slime-quit-lisp))
              (slime-kill-all-buffers)))
  (:one-liner "Quit all Lisps and close all SLIME buffers."))

(defslime-repl-shortcut slime-repl-quit ("quit")
  (:handler 'slime-quit-lisp)
  (:one-liner "Quit the current Lisp."))

(defslime-repl-shortcut slime-repl-defparameter ("defparameter" "!")
  (:handler (lambda (name value)
              (interactive (list (slime-read-symbol-name "Name (symbol): " t)
                                 (slime-read-from-minibuffer "Value: " "*")))
              (insert "(cl:defparameter " name " " value 
                      " \"REPL generated global variable.\")")
              (slime-repl-send-input t)))
  (:one-liner "Define a new global, special, variable."))

(defslime-repl-shortcut slime-repl-compile-and-load ("compile-and-load" "cl")
  (:handler (lambda (filename)
              (interactive (list (expand-file-name
                                  (read-file-name "File: " nil nil nil nil))))
              (slime-save-some-lisp-buffers)
              (slime-repl-shortcut-eval-async
               `(swank:compile-file-if-needed 
                 ,(slime-to-lisp-filename filename) t)
               (slime-make-compilation-finished-continuation (current-buffer)))))
  (:one-liner "Compile (if neccessary) and load a lisp file."))

(defslime-repl-shortcut nil  ("restart-inferior-lisp")
  (:handler 'slime-restart-inferior-lisp)
  (:one-liner "Restart *inferior-lisp* and reconnect SLIME."))

(defun slime-restart-inferior-lisp ()
  (interactive)
  (assert (slime-inferior-process) () "No inferior lisp process")
  (slime-quit-lisp-internal (slime-connection) 'slime-restart-sentinel t))

(defun slime-restart-sentinel (process message)
  "Restart the inferior lisp process.
Also rearrange windows."
  (assert (process-status process) 'closed)
  (let* ((proc (slime-inferior-process process))
         (args (slime-inferior-lisp-args proc))
         (buffer (buffer-name (process-buffer proc)))
         (buffer-window (get-buffer-window buffer))
         (new-proc (slime-start-lisp (plist-get args :program)
                                     (plist-get args :program-args)
                                     (plist-get args :env)
                                     nil
                                     buffer))
         (repl-buffer (slime-repl-buffer nil process))
         (repl-window (and repl-buffer (get-buffer-window repl-buffer))))
    (slime-net-close process)
    (slime-inferior-connect new-proc args)
    (cond ((and repl-window (not buffer-window))
           (set-window-buffer repl-window buffer)
           (select-window repl-window))
          (repl-window
           (select-window repl-window))
          (t 
           (pop-to-buffer buffer)))
    (switch-to-buffer buffer)
    (goto-char (point-max))))


;;;;; Cleanup after a quit

(defun slime-kill-all-buffers ()
  "Kill all the slime related buffers.
This is only used by the repl command sayoonara."
  (dolist (buf (buffer-list))
    (when (or (string= (buffer-name buf) slime-event-buffer-name)
              (string-match "^\\*inferior-lisp*" (buffer-name buf))
              (string-match "^\\*slime-repl .*\\*$" (buffer-name buf))
              (string-match "^\\*sldb .*\\*$" (buffer-name buf))
              (string-match "^\\*SLIME.*\\*$" (buffer-name buf)))
      (kill-buffer buf))))


;;;; Compilation and the creation of compiler-note annotations

(defvar slime-highlight-compiler-notes t
  "*When non-nil annotate buffers with compilation notes etc.")

(defcustom slime-display-compilation-output t
  "Display the REPL buffer before compiling files."
  :type '(choice (const :tag "Enable" t) (const :tag "Disable" nil))
  :group 'slime-mode)

(defvar slime-before-compile-functions nil
  "A list of function called before compiling a buffer or region.
The function receive two arguments: the beginning and the end of the 
region that will be compiled.")

(defcustom slime-compilation-finished-hook 'slime-maybe-list-compiler-notes
  "Hook called with a list of compiler notes after a compilation."
  :group 'slime-mode
  :type 'hook
  :options '(slime-maybe-list-compiler-notes
             slime-list-compiler-notes
             slime-maybe-show-xrefs-for-notes))

(defcustom slime-goto-first-note-after-compilation nil
  "When T next-note will always goto to the first note in a
final, no matter where the point is."
  :group 'slime-mode
  :type 'boolean)

(defvar slime-compilation-debug-level nil
  "When non-nil compile defuns with this debug optimization level.")

(defun slime-normalize-optimization-level (n)
  (cond ((not n) nil)
        ((> n 3) 3)
        ((< n 0) 0)
        (t n)))

(defstruct (slime-compilation-result
             (:type list)
             (:conc-name slime-compilation-result.)
             (:constructor nil)
             (:copier nil))
  tag notes results durations)

(defvar slime-last-compilation-result nil
  "The result of the most recently issued compilation.")

(defun slime-compiler-notes ()
  "Return all compiler notes, warnings, and errors."
  (slime-compilation-result.notes slime-last-compilation-result))

(defun slime-compiler-results ()
  "Return the results of the most recently issued compilations."
  (slime-compilation-result.results slime-last-compilation-result))


(defun slime-compile-and-load-file ()
  "Compile and load the buffer's file and highlight compiler notes.

Each source location that is the subject of a compiler note is
underlined and annotated with the relevant information. The commands
`slime-next-note' and `slime-previous-note' can be used to navigate
between compiler notes and to display their full details."
  (interactive)
  (slime-compile-file t))

(defun slime-compile-file (&optional load)
  "Compile current buffer's file and highlight resulting compiler notes.

See `slime-compile-and-load-file' for further details."
  (interactive)
  ;;(unless (memq major-mode slime-lisp-modes)
  ;;  (error "Only valid in lisp-mode"))
  (check-parens)
  (unless buffer-file-name
    (error "Buffer %s is not associated with a file." (buffer-name)))
  (when (and (buffer-modified-p)
             (y-or-n-p (format "Save file %s? " (buffer-file-name))))
    (save-buffer))
  (run-hook-with-args 'slime-before-compile-functions (point-min) (point-max))
  (let ((file (slime-to-lisp-filename (buffer-file-name))))
    (slime-eval-with-transcript
     `(swank:compile-file-for-emacs ,file ,(if load t nil))
     (format "Compile file %s" file)
     (not slime-display-compilation-output)
     (slime-rcurry #'slime-compilation-finished (current-buffer)))
    (message "Compiling %s..." file)))

(defun slime-compile-defun (&optional raw-prefix-arg)
  "Compile the current toplevel form. 

If invoked with a simple prefix-arg (`C-u'), compile the defun
with maximum debug setting. If invoked with a numeric prefix arg,
compile with a debug setting of that number."
  (interactive "P")
  (let* ((prefix-arg (and raw-prefix-arg (prefix-numeric-value raw-prefix-arg)))
         (debug-level (slime-normalize-optimization-level prefix-arg)) 
         (slime-compilation-debug-level debug-level))
    (apply #'slime-compile-region (slime-region-for-defun-at-point))))

(defun slime-compile-region (start end)
  "Compile the region."
  (interactive "r")
  (slime-flash-region start end)
  (run-hook-with-args 'slime-before-compile-functions start end)
  (slime-compile-string (buffer-substring-no-properties start end) start))

(defun slime-flash-region (start end &optional timeout)
  (let ((overlay (make-overlay start end))) 
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun slime-compile-string (string start-offset)
  (slime-eval-async 
   `(swank:compile-string-for-emacs
     ,string
     ,(buffer-name)
     ,start-offset
     ,(if (buffer-file-name) (file-name-directory (buffer-file-name)))
     ',slime-compilation-debug-level)
   (slime-make-compilation-finished-continuation (current-buffer))))

(defun slime-make-compilation-finished-continuation (current-buffer &optional emacs-snapshot)
  (lexical-let ((buffer current-buffer) (snapshot emacs-snapshot))
    (lambda (result)
      (slime-compilation-finished result buffer snapshot))))

(defun slime-compilation-finished (compilation-result buffer &optional emacs-snapshot)
  (with-struct (slime-compilation-result. notes durations) compilation-result
    (with-current-buffer buffer
      (setf slime-compilation-just-finished t)
      (setf slime-last-compilation-result compilation-result)
      (slime-show-note-counts notes (reduce #'+ durations))
      (when slime-highlight-compiler-notes
        (slime-highlight-notes notes)))
    (run-hook-with-args 'slime-compilation-finished-hook notes emacs-snapshot)))

(defun slime-note-count-string (severity count &optional suppress-if-zero)
  (cond ((and (zerop count) suppress-if-zero)
         "")
        (t (format "%2d %s%s " count severity (if (= count 1) "" "s")))))

(defun slime-show-note-counts (notes &optional secs)
  (let ((nerrors 0) (nwarnings 0) (nstyle-warnings 0) (nnotes 0))
    (dolist (note notes)
      (ecase (slime-note.severity note)
	((:error :read-error) (incf nerrors))
        (:warning             (incf nwarnings))
        (:style-warning       (incf nstyle-warnings))
        (:note                (incf nnotes))))
    (message "Compilation finished:%s%s%s%s%s"
             (slime-note-count-string "error" nerrors)
             (slime-note-count-string "warning" nwarnings)
             (slime-note-count-string "style-warning" nstyle-warnings t)
             (slime-note-count-string "note" nnotes)
             (if secs (format "[%.2f secs]" secs) ""))))


(defun slime-highlight-notes (notes)
  "Highlight compiler notes, warnings, and errors in the buffer."
  (interactive (list (slime-compiler-notes)))
  (with-temp-message "Highlighting notes..."
    (save-excursion
      (save-restriction
        (widen)                  ; highlight notes on the whole buffer
        (slime-remove-old-overlays)
        (mapc #'slime-overlay-note (slime-merge-notes-for-display notes))))))


(defun slime-remove-old-overlays ()
  "Delete the existing Slime overlays in the current buffer."
  (dolist (buffer (slime-filter-buffers (lambda () slime-mode)))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)                ; remove overlays within the whole buffer.
          (goto-char (point-min))
          (while (not (eobp))
            (dolist (o (overlays-at (point)))
              (when (overlay-get o 'slime)
                (delete-overlay o)))
            (goto-char (next-overlay-change (point)))))))))

(defun slime-filter-buffers (predicate)
  "Return a list of where PREDICATE returns true.
PREDICATE is executed in the buffer to test."
  (remove-if-not (lambda (%buffer)
                   (with-current-buffer %buffer
                     (funcall predicate)))
                 (buffer-list)))

;;;;; Recompilation.

(defun slime-recompile-location (location &optional debug-level)
  (save-excursion
    (slime-pop-to-location location 'excursion)
    (slime-compile-defun debug-level)))

(defun slime-recompile-locations (locations &optional debug-level)
  (let (strings buffers packages positions directories)
    (flet ((push-location-data (loc)
             (save-excursion
               (slime-pop-to-location loc 'excursion)
               (multiple-value-bind (start end) (slime-region-for-defun-at-point)
                 (push (buffer-substring-no-properties start end) strings)
                 (push (buffer-name) buffers)
                 (push (slime-current-package) packages)
                 (push start positions)
                 (push (if (buffer-file-name)
                           (file-name-directory (buffer-file-name))
                           nil)
                       directories)))))
      (mapc #'push-location-data locations)
      (slime-eval-async 
       `(swank:compile-multiple-strings-for-emacs
         ',(nreverse strings)   ',(nreverse buffers)     ',(nreverse packages)
         ',(nreverse positions) ',(nreverse directories)  ,debug-level)
       (slime-make-compilation-finished-continuation (current-buffer))))))



;;;;; Merging together compiler notes in the same location.

(defun slime-merge-notes-for-display (notes)
  "Merge together notes that refer to the same location.
This operation is \"lossy\" in the broad sense but not for display purposes."
  (mapcar #'slime-merge-notes
          (slime-group-similar 'slime-notes-in-same-location-p notes)))

(defun slime-merge-notes (notes)
  "Merge NOTES together. Keep the highest severity, concatenate the messages."
  (let* ((new-severity (reduce #'slime-most-severe notes
                               :key #'slime-note.severity))
         (new-message (mapconcat #'slime-note.short-message notes "\n")))
    (let ((new-note (copy-list (car notes))))
      (setf (getf new-note :message) new-message)
      (setf (getf new-note :severity) new-severity)
      new-note)))

;; XXX: unused function
(defun slime-intersperse (element list)
  "Intersperse ELEMENT between each element of LIST."
  (if (null list) 
      '()
    (cons (car list)
          (mapcan (lambda (x) (list element x)) (cdr list)))))

(defun slime-notes-in-same-location-p (a b)
  (equal (slime-note.location a) (slime-note.location b)))

(defun slime-group-similar (similar-p list)
  "Return the list of lists of 'similar' adjacent elements of LIST.
The function SIMILAR-P is used to test for similarity.
The order of the input list is preserved."
  (if (null list)
      nil
    (let ((accumulator (list (list (car list)))))
      (dolist (x (cdr list))
        (if (funcall similar-p x (caar accumulator))
            (push x (car accumulator))
          (push (list x) accumulator)))
      (reverse (mapcar #'reverse accumulator)))))


;;;;; Compiler notes list

(defun slime-one-line-ify (string)
  "Return a single-line version of STRING.
Each newlines and following indentation is replaced by a single space."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "\n[\n \t]*" nil t)
      (replace-match " "))
    (buffer-string)))

(defun slime-xrefs-for-notes (notes)
  (let ((xrefs))
    (dolist (note notes)
      (let* ((location (getf note :location))
             (fn (cadr (assq :file (cdr location))))
             (file (assoc fn xrefs))
             (node
              (cons (format "%s: %s" 
                            (getf note :severity)
                            (slime-one-line-ify (getf note :message)))
                    location)))
        (when fn
          (if file
              (push node (cdr file))
              (setf xrefs (acons fn (list node) xrefs))))))
    xrefs))

(defun slime-maybe-show-xrefs-for-notes (&optional notes emacs-snapshot)
  "Show the compiler notes NOTES if they come from more than one file."
  (let* ((notes (or notes (slime-compiler-notes))) 
         (xrefs (slime-xrefs-for-notes notes)))
    (when (slime-length> xrefs 1)          ; >1 file
      (slime-show-xrefs
       xrefs 'definition "Compiler notes" (slime-current-package)
       emacs-snapshot))))

(defun slime-note-has-location-p (note)
  (not (eq ':error (car (slime-note.location note)))))

(defun slime-maybe-list-compiler-notes (notes &optional emacs-snapshot)
  "Show the compiler notes if appropriate."
  ;; don't pop up a buffer if all notes are already annotated in the
  ;; buffer itself
  (unless (every #'slime-note-has-location-p notes)
    (slime-list-compiler-notes notes emacs-snapshot)))

(defun slime-list-compiler-notes (notes &optional emacs-snapshot)
  "Show the compiler notes NOTES in tree view."
  (interactive (list (slime-compiler-notes)))
  (with-temp-message "Preparing compiler note tree..."
    (slime-with-popup-buffer ("*SLIME Compiler-Notes*" nil nil emacs-snapshot)
      (erase-buffer)
      (slime-compiler-notes-mode)
      (when (null notes)
        (insert "[no notes]"))
      (let ((collapsed-p))
        (dolist (tree (slime-compiler-notes-to-tree notes))
          (when (slime-tree.collapsed-p tree) (setf collapsed-p t))
          (slime-tree-insert tree "")
          (insert "\n"))
        (goto-char (point-min))))))

(defun slime-alistify (list key test)
  "Partition the elements of LIST into an alist.
KEY extracts the key from an element and TEST is used to compare
keys."
  (declare (type function key))
  (let ((alist '()))
    (dolist (e list)
      (let* ((k (funcall key e))
	     (probe (assoc* k alist :test test)))
	(if probe
	    (push e (cdr probe))
            (push (cons k (list e)) alist))))
    ;; Put them back in order.
    (loop for (key . value) in (reverse alist)
          collect (cons key (reverse value)))))

(defun slime-note.severity (note)
  (plist-get note :severity))

(defun slime-note.message (note)
  (plist-get note :message))

(defun slime-note.short-message (note)
  (or (plist-get note :short-message)
      (plist-get note :message)))

(defun slime-note.location (note)
  (plist-get note :location))

(defun slime-severity-label (severity)
  (ecase severity
    (:note "Notes")
    (:warning "Warnings")
    (:error "Errors")
    (:read-error "Read Errors")
    (:style-warning "Style Warnings")))

(defvar slime-tree-printer 'slime-tree-default-printer)

(defun slime-tree-for-note (note)
  (make-slime-tree :item (slime-note.message note)
                   :plist (list 'note note)
                   :print-fn slime-tree-printer))

(defun slime-tree-for-severity (severity notes collapsed-p)
  (make-slime-tree :item (format "%s (%d)" 
                                 (slime-severity-label severity)
                                 (length notes))
                   :kids (mapcar #'slime-tree-for-note notes)
                   :collapsed-p collapsed-p))

(defun slime-compiler-notes-to-tree (notes)
  (let* ((alist (slime-alistify notes #'slime-note.severity #'eq))
         (collapsed-p (slime-length> alist 1)))
    (loop for (severity . notes) in alist
          collect (slime-tree-for-severity severity notes 
                                           collapsed-p))))

(defvar slime-compiler-notes-mode-map)

(define-derived-mode slime-compiler-notes-mode fundamental-mode 
  "Compiler-Notes"
  "\\<slime-compiler-notes-mode-map>\
\\{slime-compiler-notes-mode-map}
\\{slime-popup-buffer-mode-map}
"
  (slime-set-truncate-lines))

(slime-define-keys slime-compiler-notes-mode-map
  ((kbd "RET") 'slime-compiler-notes-default-action-or-show-details)
  ([return] 'slime-compiler-notes-default-action-or-show-details)
  ([mouse-2] 'slime-compiler-notes-default-action-or-show-details/mouse))

(defun slime-compiler-notes-default-action-or-show-details/mouse (event)
  "Invoke the action pointed at by the mouse, or show details."
  (interactive "e")
  (destructuring-bind (mouse-2 (w pos &rest _) &rest __) event
    (save-excursion
      (goto-char pos)
      (let ((fn (get-text-property (point) 
                                   'slime-compiler-notes-default-action)))
	(if fn (funcall fn) (slime-compiler-notes-show-details))))))

(defun slime-compiler-notes-default-action-or-show-details ()
  "Invoke the action at point, or show details."
  (interactive)
  (let ((fn (get-text-property (point) 'slime-compiler-notes-default-action)))
    (if fn (funcall fn) (slime-compiler-notes-show-details))))

(defun slime-compiler-notes-show-details ()
  (interactive)
  (let* ((tree (slime-tree-at-point))
         (note (plist-get (slime-tree.plist tree) 'note))
         (inhibit-read-only t))
    (cond ((not (slime-tree-leaf-p tree))
           (slime-tree-toggle tree))
          (t
           (slime-show-source-location (slime-note.location note) t)))))


;;;;;; Tree Widget

(defstruct (slime-tree (:conc-name slime-tree.))
  item
  (print-fn #'slime-tree-default-printer :type function)
  (kids '() :type list)
  (collapsed-p t :type boolean)
  (prefix "" :type string)
  (start-mark nil)
  (end-mark nil)
  (plist '() :type list))

(defun slime-tree-leaf-p (tree)
  (not (slime-tree.kids tree)))

(defun slime-tree-default-printer (tree)
  (princ (slime-tree.item tree) (current-buffer)))

(defun slime-tree-decoration (tree)
  (cond ((slime-tree-leaf-p tree) "-- ")
	((slime-tree.collapsed-p tree) "[+] ")
	(t "-+  ")))

(defun slime-tree-insert-list (list prefix)
  "Insert a list of trees."
  (loop for (elt . rest) on list 
	do (cond (rest
		  (insert prefix " |")
		  (slime-tree-insert elt (concat prefix " |"))
                  (insert "\n"))
		 (t
		  (insert prefix " `")
		  (slime-tree-insert elt (concat prefix "  "))))))

(defun slime-tree-insert-decoration (tree)
  (insert (slime-tree-decoration tree)))

(defun slime-tree-indent-item (start end prefix)
  "Insert PREFIX at the beginning of each but the first line.
This is used for labels spanning multiple lines."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (while (< start (point))
      (insert-before-markers prefix)
      (forward-line -1))))

(defun slime-tree-insert (tree prefix)
  "Insert TREE prefixed with PREFIX at point."
  (with-struct (slime-tree. print-fn kids collapsed-p start-mark end-mark) tree
    (let ((line-start (line-beginning-position)))
      (setf start-mark (point-marker))
      (slime-tree-insert-decoration tree)
      (funcall print-fn tree)
      (slime-tree-indent-item start-mark (point) (concat prefix "   "))
      (add-text-properties line-start (point) (list 'slime-tree tree))
      (set-marker-insertion-type start-mark t)
      (when (and kids (not collapsed-p))
        (terpri (current-buffer))
        (slime-tree-insert-list kids prefix))
      (setf (slime-tree.prefix tree) prefix)
      (setf end-mark (point-marker)))))

(defun slime-tree-at-point ()
  (cond ((get-text-property (point) 'slime-tree))
        (t (error "No tree at point"))))

(defun slime-tree-delete (tree)
  "Delete the region for TREE."
  (delete-region (slime-tree.start-mark tree)
                 (slime-tree.end-mark tree)))

(defun slime-tree-toggle (tree)
  "Toggle the visibility of TREE's children."
  (with-struct (slime-tree. collapsed-p start-mark end-mark prefix) tree
    (setf collapsed-p (not collapsed-p))
    (slime-tree-delete tree)
    (insert-before-markers " ") ; move parent's end-mark
    (backward-char 1)
    (slime-tree-insert tree prefix)
    (delete-char 1)
    (goto-char start-mark)))


;;;;; Adding a single compiler note

(defun slime-overlay-note (note)
  "Add a compiler note to the buffer as an overlay.
If an appropriate overlay for a compiler note in the same location
already exists then the new information is merged into it. Otherwise a
new overlay is created."
  (multiple-value-bind (start end) (slime-choose-overlay-region note)
    (when start
      (goto-char start)
      (let ((severity (plist-get note :severity))
            (message (plist-get note :message))
            (overlay (slime-note-at-point)))
        (if overlay
            (slime-merge-note-into-overlay overlay severity message)
            (slime-create-note-overlay note start end severity message))))))

(defun slime-create-note-overlay (note start end severity message)
  "Create an overlay representing a compiler note.
The overlay has several properties:
  FACE       - to underline the relevant text.
  SEVERITY   - for future reference, :NOTE, :STYLE-WARNING, :WARNING, or :ERROR.
  MOUSE-FACE - highlight the note when the mouse passes over.
  HELP-ECHO  - a string describing the note, both for future reference
               and for display as a tooltip (due to the special
               property name)."
  (let ((overlay (make-overlay start end)))
    (flet ((putp (name value) (overlay-put overlay name value)))
      (putp 'slime note)
      (putp 'face (slime-severity-face severity))
      (putp 'severity severity)
      (unless (slime-emacs-20-p)
	(putp 'mouse-face 'highlight))
      (putp 'help-echo message)
      overlay)))

;; XXX Obsolete due to `slime-merge-notes-for-display' doing the
;; work already -- unless we decide to put several sets of notes on a
;; buffer without clearing in between, which only this handles.
(defun slime-merge-note-into-overlay (overlay severity message)
  "Merge another compiler note into an existing overlay.
The help text describes both notes, and the highest of the severities
is kept."
  (flet ((putp (name value) (overlay-put overlay name value))
	 (getp (name)       (overlay-get overlay name)))
    (putp 'severity (slime-most-severe severity (getp 'severity)))
    (putp 'face (slime-severity-face (getp 'severity)))
    (putp 'help-echo (concat (getp 'help-echo) "\n" message))))

(defun slime-choose-overlay-region (note)
  "Choose the start and end points for an overlay over NOTE.
If the location's sexp is a list spanning multiple lines, then the
region around the first element is used.
Return nil if there's no useful source location."
  (let ((location (slime-note.location note)))
    (when location 
      (destructure-case location
        ((:error _) _ nil)                 ; do nothing
        ((:location file pos _hints)
         (cond ((eq (car file) ':source-form) nil)
               (t
                (destructure-case pos
                  ((:position pos &optional alignp)
                   (if (eq (slime-note.severity note) :read-error)
                       (values pos (1+ pos))
                     (slime-choose-overlay-for-sexp location)))
                  (t 
                   (slime-choose-overlay-for-sexp location))))))))))
          
(defun slime-choose-overlay-for-sexp (location)
  (slime-goto-source-location location)
  (skip-chars-forward "'#`")
  (let ((start (point)))
    (ignore-errors (slime-forward-sexp))
    (if (slime-same-line-p start (point))
        (values start (point))
      (values (1+ start)
              (progn (goto-char (1+ start))
                     (ignore-errors (forward-sexp 1))
                     (point))))))

(defun slime-same-line-p (pos1 pos2)
  "Return t if buffer positions POS1 and POS2 are on the same line."
  (save-excursion (goto-char (min pos1 pos2))
                  (<= (max pos1 pos2) (line-end-position))))

(defun slime-severity-face (severity)
  "Return the name of the font-lock face representing SEVERITY."
  (ecase severity
    (:error         'slime-error-face)
    (:read-error    'slime-error-face)
    (:warning       'slime-warning-face)
    (:style-warning 'slime-style-warning-face)
    (:note          'slime-note-face)))

(defun slime-most-severe (sev1 sev2)
  "Return the most servere of two conditions.
Severity is ordered as :NOTE < :STYLE-WARNING < :WARNING < :ERROR."
                                        ; Well, not exactly Smullyan..
  (let ((order '(:note :style-warning :warning :error :read-error)))
    (if (>= (position sev1 order) 
            (position sev2 order))
        sev1
      sev2)))

;; XXX: unused function
(defun slime-visit-source-path (source-path)
  "Visit a full source path including the top-level form."
  (goto-char (point-min))
  (slime-forward-source-path source-path))

(defun slime-forward-positioned-source-path (source-path)
  "Move forward through a sourcepath from a fixed position.
The point is assumed to already be at the outermost sexp, making the
first element of the source-path redundant."
  (ignore-errors 
    (slime-forward-sexp)
    (beginning-of-defun))
  (when-let (source-path (cdr source-path))
    (down-list 1)
    (slime-forward-source-path source-path)))

(defun slime-forward-source-path (source-path)
  (let ((origin (point)))
    (condition-case nil
        (progn
          (loop for (count . more) on source-path
                do (progn
                     (slime-forward-sexp count)
                     (when more (down-list 1))))
          ;; Align at beginning
          (slime-forward-sexp)
          (beginning-of-sexp))
      (error (goto-char origin)))))

(defun slime-filesystem-toplevel-directory ()
  ;; Windows doesn't have a true toplevel root directory, and all
  ;; filenames look like "c:/foo/bar/quux.baz" from an Emacs
  ;; perspective anyway.
  (if (memq system-type '(ms-dos windows-nt))
      ""
      (file-name-as-directory "/")))

(defun slime-file-name-merge-source-root (target-filename buffer-filename)
  "Returns a filename where the source root directory of TARGET-FILENAME
is replaced with the source root directory of BUFFER-FILENAME.

If no common source root could be determined, return NIL.

E.g. (slime-file-name-merge-source-root
       \"/usr/local/src/joe/upstream/sbcl/code/late-extensions.lisp\"
       \"/usr/local/src/joe/hacked/sbcl/compiler/deftype.lisp\")
 
        ==> \"/usr/local/src/joe/hacked/sbcl/code/late-extensions.lisp\"
"
  (let ((target-dirs (slime-split-string (file-name-directory target-filename) "/" t))
        (buffer-dirs (slime-split-string (file-name-directory buffer-filename) "/" t)))
    ;; Starting from the end, we look if one of the TARGET-DIRS exists
    ;; in BUFFER-FILENAME---if so, it and everything left from that dirname
    ;; is considered to be the source root directory of BUFFER-FILENAME.
    (loop with target-suffix-dirs = nil
          with buffer-dirs* = (reverse buffer-dirs)
          with target-dirs* = (reverse target-dirs)
          for target-dir in target-dirs*
          do (flet ((concat-dirs (dirs)
                      (apply #'concat (mapcar #'file-name-as-directory dirs))))
               (let ((pos (position target-dir buffer-dirs* :test #'equal)))
                 (if (not pos)    ; TARGET-DIR not in BUFFER-FILENAME?
                     (push target-dir target-suffix-dirs)
                     (let* ((target-suffix (concat-dirs target-suffix-dirs)) ; PUSH reversed for us!
                            (buffer-root   (concat-dirs (reverse (nthcdr pos buffer-dirs*)))))
                       (return (concat (slime-filesystem-toplevel-directory)
                                       buffer-root
                                       target-suffix
                                       (file-name-nondirectory target-filename))))))))))

(defun slime-highlight-differences-in-dirname (base-dirname contrast-dirname)
  "Returns a copy of BASE-DIRNAME where all differences between
BASE-DIRNAME and CONTRAST-DIRNAME are propertized with a
highlighting face."
  (setq base-dirname (file-name-as-directory base-dirname))
  (setq contrast-dirname (file-name-as-directory contrast-dirname))
  (flet ((insert-dir (dirname)
           (insert (file-name-as-directory dirname)))
         (insert-dir/propzd (dirname)
           (slime-insert-propertized '(face highlight) dirname)
           (insert "/")))  ; Not exactly portable (to VMS...)
    (let ((base-dirs (slime-split-string base-dirname "/" t))
          (contrast-dirs (slime-split-string contrast-dirname "/" t)))
      (with-temp-buffer
        (loop initially (insert (slime-filesystem-toplevel-directory))
              for base-dir in base-dirs do
              (let ((pos (position base-dir contrast-dirs :test #'equal)))
                (if (not pos)
                    (insert-dir/propzd base-dir)
                    (progn (insert-dir base-dir)
                           (setq contrast-dirs (nthcdr (1+ pos) contrast-dirs))))))
        (buffer-substring (point-min) (point-max))))))

(defvar slime-warn-when-possibly-tricked-by-M-. t
  "When working on multiple source trees simultaneously, the way
`slime-edit-definition' (M-.) works can sometimes be confusing:

`M-.' visits locations that are present in the current Lisp image,
which works perfectly well as long as the image reflects the source
tree that one is currently looking at.

In the other case, however, one can easily end up visiting a file
in a different source root directory (the one corresponding to
the Lisp image), and is thus easily tricked to modify the wrong
source files---which can lead to quite some stressfull cursing.

If this variable is T, a warning message is issued to raise the
user's attention whenever `M-.' is about opening a file in a
different source root that also exists in the source root
directory of the user's current buffer.

There's no guarantee that all possible cases are covered, but
if you encounter such a warning, it's a strong indication that
you should check twice before modifying.")

(defun slime-maybe-warn-for-different-source-root (target-filename buffer-filename)
  (when slime-warn-when-possibly-tricked-by-M-.
    (let ((guessed-target (slime-file-name-merge-source-root target-filename
                                                             buffer-filename)))
      (when (and guessed-target
                 (not (equal guessed-target target-filename))
                 (file-exists-p guessed-target))
        (slime-message "Attention: This is `%s'."
                       (concat (slime-highlight-differences-in-dirname
                                 (file-name-directory target-filename)
                                 (file-name-directory guessed-target))
                               (file-name-nondirectory target-filename)))))))


(defun slime-goto-location-buffer (buffer)
  (flet ((file-truename-safe (filename) (and filename (file-truename filename))))
    (destructure-case buffer
      ((:file filename)
       (let ((target-filename (file-truename-safe (slime-from-lisp-filename filename)))
             (buffer-filename (file-truename-safe (buffer-file-name))))
         (when buffer-filename
           (slime-maybe-warn-for-different-source-root target-filename buffer-filename))
         (unless (and buffer-filename (string= buffer-filename target-filename))
           (set-buffer (find-file-noselect target-filename t))))
       (goto-char (point-min)))
      ((:buffer buffer-name)
       (let ((old-buffer-filename (file-truename-safe (buffer-file-name)))
             (target-buffer-filename (file-truename-safe
                                      (buffer-file-name (get-buffer buffer-name)))))
         (when (and target-buffer-filename old-buffer-filename)
           (slime-maybe-warn-for-different-source-root target-buffer-filename
                                                       old-buffer-filename)))
       (set-buffer buffer-name)
       (goto-char (point-min)))
      ((:source-form string)
       (set-buffer (get-buffer-create "*SLIME Source Form*"))
       (erase-buffer)
       (lisp-mode)
       (insert string)
       (goto-char (point-min)))
      ((:zip file entry)
       (require 'arc-mode)
       (set-buffer (find-file-noselect file t))
       (goto-char (point-min))
       (re-search-forward (concat "  " entry "$"))
       (let ((buffer (save-window-excursion
                       (archive-extract)
                       (current-buffer))))
         (set-buffer buffer)
         (goto-char (point-min)))))))

(defun slime-goto-location-position (position)
  (destructure-case position
    ((:position pos)
     (goto-char 1)
     (forward-char (- (1- pos) (slime-eol-conversion-fixup (1- pos)))))
    ((:offset start offset)
     (goto-char start)
     (forward-char offset))
    ((:line start &optional column)
     (goto-line start)
     (cond (column (move-to-column column))
           (t (skip-chars-forward " \t"))))
    ((:function-name name)
     (let ((case-fold-search t)
           (name (regexp-quote name)))
       (or 
        (re-search-forward 
         (format "\\s *(def\\(\\s_\\|\\sw\\)*\\s +%s\\S_" name) nil t)
        (re-search-forward 
         (format "\\s *(def\\(\\s_\\|\\sw\\)*\\s +(*%s\\S_" name) nil t)
        (re-search-forward 
         (format "[( \t]%s\\>\\(\\s \\|$\\)" name) nil t)))
     (goto-char (match-beginning 0)))
    ((:method name specializers &rest qualifiers)
     (slime-search-method-location name specializers qualifiers))
    ((:source-path source-path start-position)
     (cond (start-position
            (goto-char start-position)
            (slime-forward-positioned-source-path source-path))
           (t
            (slime-forward-source-path source-path))))))

(defun slime-eol-conversion-fixup (n)
  ;; Return the number of \r\n eol markers that we need to cross when
  ;; moving N chars forward.  N is the number of chars but \r\n are
  ;; counted as 2 separate chars.
  (case (coding-system-eol-type buffer-file-coding-system)
    ((1) 
     (save-excursion 
       (do ((pos (+ (point) n))
            (count 0 (1+ count)))
           ((>= (point) pos) (1- count))
         (forward-line)
         (decf pos))))
    (t 0)))

(defun slime-search-method-location (name specializers qualifiers)
  ;; Look for a sequence of words (def<something> method name
  ;; qualifers specializers don't look for "T" since it isn't requires
  ;; (arg without t) as class is taken as such.
  (let* ((case-fold-search t)
         (name (regexp-quote name))
         (qualifiers (mapconcat (lambda (el) (concat ".+?\\<" el "\\>"))
                                qualifiers ""))
         (specializers (mapconcat (lambda (el) 
                                    (if (eql (aref el 0) ?\()
                                        (let ((spec (read el)))
                                          (if (eq (car spec) 'EQL)
                                              (concat ".*?\\n\\{0,1\\}.*?(EQL.*?'\\{0,1\\}"
                                                      (format "%s" (second spec)) ")")
                                            (error "don't understand specializer: %s,%s" el (car spec))))
                                      (concat ".+?\n\\{0,1\\}.+?\\<" el "\\>")))
                                  (remove "T" specializers) ""))
         (regexp (format "\\s *(def\\(\\s_\\|\\sw\\)*\\s +%s\\s +%s%s" name
                         qualifiers specializers)))
    (or (and (re-search-forward regexp  nil t)
             (goto-char (match-beginning 0)))
        ;;	(slime-goto-location-position `(:function-name ,name))
        )))

(defun slime-search-call-site (fname)
  "Move to the place where FNAME called.
Don't move if there are multiple or no calls in the current defun."
  (save-restriction 
    (narrow-to-defun)
    (let ((start (point))
          (regexp (concat "(" fname "[\n \t]")))
      (cond ((and (re-search-forward regexp nil t)
                  (not (re-search-forward regexp nil t)))
             (goto-char (match-beginning 0)))
            (t (goto-char start))))))

(defun slime-goto-source-location (location &optional noerror)
  "Move to the source location LOCATION.  Several kinds of locations
are supported:

<location> ::= (:location <buffer> <position> <hints>)
             | (:error <message>) 

<buffer>   ::= (:file <filename>)
             | (:buffer <buffername>)
             | (:source-form <string>)
             | (:zip <file> <entry>)

<position> ::= (:position <fixnum>) ; 1 based (for files)
             | (:offset <start> <offset>) ; start+offset (for C-c C-c)
             | (:line <line> [<column>])
             | (:function-name <string>)
             | (:source-path <list> <start-position>) 
             | (:method <name string> <specializer strings> . <qualifiers strings>)"
  (destructure-case location
    ((:location buffer position hints)
     (slime-goto-location-buffer buffer)
     (let ((pos (slime-location-offset location)))
       (cond ((and (<= (point-min) pos) (<= pos (point-max))))
             (widen-automatically (widen))
             (t (error "Location is outside accessible part of buffer")))
       (goto-char pos)))
    ((:error message)
     (if noerror
         (slime-message "%s" message)
       (error "%s" message)))))

(defun slime-location-offset (location)
  "Return the position, as character number, of LOCATION."
  (save-restriction
    (widen)
    (slime-goto-location-position (slime-location.position location))
    (let ((hints (slime-location.hints location)))
      (when-let (snippet (getf hints :snippet))
        (slime-isearch snippet))
      (when-let (fname (getf hints :call-site))
        (slime-search-call-site fname))
      (when (getf hints :align)
        (slime-forward-sexp)
        (beginning-of-sexp)))
    (point)))


;;;;; Incremental search
;;
;; Search for the longest match of a string in either direction.
;;
;; This is for locating text that is expected to be near the point and
;; may have been modified (but hopefully not near the beginning!)

(defun slime-isearch (string)
  "Find the longest occurence of STRING either backwards of forwards.
If multiple matches exist the choose the one nearest to point."
  (goto-char
   (let* ((start (point))
          (len1 (slime-isearch-with-function 'search-forward string))
          (pos1 (point)))
     (goto-char start)
     (let* ((len2 (slime-isearch-with-function 'search-backward string))
            (pos2 (point)))
       (cond ((and len1 len2)
              ;; Have a match in both directions
              (cond ((= len1 len2)
                     ;; Both are full matches -- choose the nearest.
                     (if (< (abs (- start pos1))
                            (abs (- start pos2)))
                         pos1 pos2))
                    ((> len1 len2) pos1)
                    ((> len2 len1) pos2)))
             (len1 pos1)
             (len2 pos2)
             (t start))))))

(defun slime-isearch-with-function (search-fn string)
  "Search for the longest substring of STRING using SEARCH-FN.
SEARCH-FN is either the symbol `search-forward' or `search-backward'."
  (unless (string= string "")
    (loop for i from 1 to (length string)
          while (funcall search-fn (substring string 0 i) nil t)
          for match-data = (match-data)
          do (case search-fn
               (search-forward  (goto-char (match-beginning 0)))
               (search-backward (goto-char (1+ (match-end 0)))))
          finally (return (if (null match-data)
                              nil
                            ;; Finish based on the last successful match
                            (store-match-data match-data)
                            (goto-char (match-beginning 0))
                            (- (match-end 0) (match-beginning 0)))))))


;;;;; Visiting and navigating the overlays of compiler notes

(defvar slime-compilation-just-finished nil
  "A buffer local variable which is T when we've just compiled a
buffer and haven't yet started navigating its notes.")
(make-variable-buffer-local 'slime-compilation-just-finished)

(defun slime-next-note ()
  "Go to and describe the next compiler note in the buffer."
  (interactive)
  (let ((here (point)))
    (when (and slime-goto-first-note-after-compilation
               slime-compilation-just-finished)
      (goto-char (point-min))
      (setf slime-compilation-just-finished nil))
    (slime-find-next-note)
    (if (slime-note-at-point)
        (slime-show-note (slime-note-at-point))
        (progn
          (goto-char here)
          (message "No next note.")))))

(defun slime-previous-note ()
  "Go to and describe the previous compiler note in the buffer."
  (interactive)
  (let ((here (point)))
    (when (and slime-goto-first-note-after-compilation
               slime-compilation-just-finished)
      (goto-char (point-max))
      (setf slime-compilation-just-finished nil))
    (slime-find-previous-note)
    (if (slime-note-at-point)
        (slime-show-note (slime-note-at-point))
        (progn
          (goto-char here)
          (message "No previous note.")))))

(defun slime-remove-notes ()
  "Remove compiler-note annotations from the current buffer."
  (interactive)
  (slime-remove-old-overlays))

(defun slime-show-note (overlay)
  "Present the details of a compiler note to the user."
  (slime-temporarily-highlight-note overlay)
  (let ((message (get-char-property (point) 'help-echo)))
    (slime-message "%s" (if (zerop (length message)) "\"\"" message))))

(defun slime-temporarily-highlight-note (overlay)
  "Temporarily highlight a compiler note's overlay.
The highlighting is designed to both make the relevant source more
visible, and to highlight any further notes that are nested inside the
current one.

The highlighting is automatically undone before the next Emacs command."
  (lexical-let ((old-face (overlay-get overlay 'face))
                (overlay overlay))
    (push (lambda () (overlay-put overlay 'face old-face))
	  slime-pre-command-actions)
    (overlay-put overlay 'face 'slime-highlight-face)))


;;;;; Overlay lookup operations

(defun slime-note-at-point ()
  "Return the overlay for a note starting at point, otherwise NIL."
  (find (point) (slime-note-overlays-at-point)
	:key 'overlay-start))

(defun slime-note-overlay-p (overlay)
  "Return true if OVERLAY represents a compiler note."
  (overlay-get overlay 'slime))

(defun slime-note-overlays-at-point ()
  "Return a list of all note overlays that are under the point."
  (remove-if-not 'slime-note-overlay-p (overlays-at (point))))

(defun slime-find-next-note ()
  "Go to the next position with the `slime-note' text property.
Retuns true if such a position is found."
  (slime-find-note 'next-single-char-property-change))

(defun slime-find-previous-note ()
  "Go to the next position with the `slime' text property.
Returns true if such a position is found."
  (slime-find-note 'previous-single-char-property-change))

(defun slime-find-note (next-candidate-fn)
  "Seek out the beginning of a note.
NEXT-CANDIDATE-FN is called to find each new position for consideration."
  (let ((origin (point)))
    (loop do (goto-char (funcall next-candidate-fn (point) 'slime))
	  until (or (slime-note-at-point)
		    (eobp)
		    (bobp)))
    (unless (slime-note-at-point)
      (goto-char origin))))


;;;; Arglist Display

(defun slime-space (n)
  "Insert a space and print some relevant information (function arglist).
Designed to be bound to the SPC key.  Prefix argument can be used to insert
more than one space."
  (interactive "p")
  (self-insert-command n)
  (when (and slime-space-information-p
             (slime-background-activities-enabled-p))
    (slime-echo-arglist)))

(defvar slime-echo-arglist-function 'slime-show-arglist)

(defun slime-echo-arglist ()
  "Display the arglist of the current form in the echo area."
  (funcall slime-echo-arglist-function))

(defun slime-show-arglist ()
  (let ((op (slime-operator-before-point)))
    (when op 
      (slime-eval-async `(swank:operator-arglist ,op ,(slime-current-package))
			(lambda (arglist)
			  (when arglist
			    (slime-message "%s" arglist)))))))

(defun slime-operator-before-point ()
  (ignore-errors 
    (save-excursion
      (backward-up-list 1)
      (down-list 1)
      (slime-symbol-name-at-point))))


;;;; Completion

;; XXX those long names are ugly to read; long names an indicator for
;; bad factoring?

(defvar slime-completions-buffer-name "*Completions*")

(make-variable-buffer-local
 (defvar slime-complete-saved-window-configuration nil
   "Window configuration before we show the *Completions* buffer.
This is buffer local in the buffer where the completion is
performed."))

(make-variable-buffer-local
 (defvar slime-completions-window nil
   "The window displaying *Completions* after saving window configuration.
If this window is no longer active or displaying the completions
buffer then we can ignore `slime-complete-saved-window-configuration'."))

(defun slime-complete-maybe-save-window-configuration ()
  "Maybe save the current window configuration.
Return true if the configuration was saved."
  (unless (or slime-complete-saved-window-configuration
              (get-buffer-window slime-completions-buffer-name))
    (setq slime-complete-saved-window-configuration
          (current-window-configuration))
    t))

(defun slime-complete-delay-restoration ()
  (slime-add-local-hook 'pre-command-hook
                        'slime-complete-maybe-restore-window-configuration))

(defun slime-complete-forget-window-configuration ()
  (setq slime-complete-saved-window-configuration nil)
  (setq slime-completions-window nil))

(defun slime-complete-restore-window-configuration ()
  "Restore the window config if available."
  (remove-hook 'pre-command-hook
               'slime-complete-maybe-restore-window-configuration)
  (when (and slime-complete-saved-window-configuration
             (slime-completion-window-active-p))
    ;; XEmacs does not allow us to restore a window configuration from
    ;; pre-command-hook, so we do it asynchronously.
    (slime-run-when-idle
     (lambda ()
       (save-excursion
         (set-window-configuration
          slime-complete-saved-window-configuration))
       (setq slime-complete-saved-window-configuration nil)
       (when (buffer-live-p slime-completions-buffer-name)
         (kill-buffer slime-completions-buffer-name))))))

(defun slime-complete-maybe-restore-window-configuration ()
  "Restore the window configuration, if the following command
terminates a current completion."
  (remove-hook 'pre-command-hook
               'slime-complete-maybe-restore-window-configuration)
  (condition-case err
      (cond ((find last-command-char "()\"'`,# \r\n:")
             (slime-complete-restore-window-configuration))
            ((not (slime-completion-window-active-p))
             (slime-complete-forget-window-configuration))
            (t
             (slime-complete-delay-restoration)))
    (error
     ;; Because this is called on the pre-command-hook, we mustn't let
     ;; errors propagate.
     (message "Error in slime-complete-restore-window-configuration: %S" err))))

(defun slime-completion-window-active-p ()
  "Is the completion window currently active?"
  (and (window-live-p slime-completions-window)
       (equal (buffer-name (window-buffer slime-completions-window))
              slime-completions-buffer-name)))

(defun slime-display-completion-list (completions base)
  (let ((savedp (slime-complete-maybe-save-window-configuration)))
    (with-output-to-temp-buffer slime-completions-buffer-name
      (display-completion-list completions)
      (let ((offset (- (point) 1 (length base))))
        (with-current-buffer standard-output
          (setq completion-base-size offset)
          (set-syntax-table lisp-mode-syntax-table))))
    (when savedp
      (setq slime-completions-window
            (get-buffer-window slime-completions-buffer-name)))))
  
(defun slime-display-or-scroll-completions (completions base)
  (cond ((and (eq last-command this-command)
              (slime-completion-window-active-p))
         (slime-scroll-completions))
        (t
         (slime-display-completion-list completions base)))
  (slime-complete-delay-restoration))

(defun slime-scroll-completions ()
  (let ((window slime-completions-window))
    (with-current-buffer (window-buffer window)
      (if (pos-visible-in-window-p (point-max) window)
          (set-window-start window (point-min))
        (save-selected-window
          (select-window window)
          (scroll-up))))))

(defun slime-complete-symbol ()
  "Complete the symbol at point.

Completion is performed by `slime-complete-symbol-function'."
  (interactive)
  (funcall slime-complete-symbol-function))

(defun slime-simple-complete-symbol ()
  "Complete the symbol at point.  
Perform completion more similar to Emacs' complete-symbol."
  (or (slime-maybe-complete-as-filename)
      (let* ((end (point))
             (beg (slime-symbol-start-pos))
             (prefix (buffer-substring-no-properties beg end))
             (result (slime-simple-completions prefix)))
        (destructuring-bind (completions partial) result
          (if (null completions)
              (progn (slime-minibuffer-respecting-message
                      "Can't find completion for \"%s\"" prefix)
                     (ding)
                     (slime-complete-restore-window-configuration))
            (insert-and-inherit (substring partial (length prefix)))
            (cond ((slime-length= completions 1)
                   (slime-minibuffer-respecting-message "Sole completion")
                   (slime-complete-restore-window-configuration))
                  ;; Incomplete
                  (t
                   (slime-minibuffer-respecting-message
                    "Complete but not unique")
                   (slime-display-or-scroll-completions completions
                                                        partial))))))))

(defun slime-maybe-complete-as-filename ()
  "If point is at a string starting with \", complete it as filename.
Return nil iff if point is not at filename."
  (if (save-excursion (re-search-backward "\"[^ \t\n]+\\=" nil t))
      (let ((comint-completion-addsuffix '("/" . "\"")))
        (if slime-when-complete-filename-expand
            (comint-replace-by-expanded-filename)
          (comint-dynamic-complete-as-filename))
        t)
    nil))

(defun slime-minibuffer-respecting-message (format &rest format-args)
  "Display TEXT as a message, without hiding any minibuffer contents."
  (let ((text (format " [%s]" (apply #'format format format-args))))
    (if (minibuffer-window-active-p (minibuffer-window))
        (if (fboundp 'temp-minibuffer-message) ;; XEmacs
            (temp-minibuffer-message text)
          (minibuffer-message text))
      (message "%s" text))))

(defvar slime-read-expression-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\t" 'slime-complete-symbol)
    (define-key map "\M-\t" 'slime-complete-symbol)
    map)
  "Minibuffer keymap used for reading CL expressions.")

(defvar slime-read-expression-history '()
  "History list of expressions read from the minibuffer.")
 
(defun slime-read-from-minibuffer (prompt &optional initial-value)
  "Read a string from the minibuffer, prompting with PROMPT.  
If INITIAL-VALUE is non-nil, it is inserted into the minibuffer before
reading input.  The result is a string (\"\" if no input was given)."
  (let ((minibuffer-setup-hook 
         (cons (lexical-let ((package (slime-current-package))
                             (connection (slime-connection)))
                 (lambda ()
                   (setq slime-buffer-package package)
                   (setq slime-buffer-connection connection)
                   (set-syntax-table lisp-mode-syntax-table)))
	       minibuffer-setup-hook)))
    (read-from-minibuffer prompt initial-value slime-read-expression-map
			  nil 'slime-read-expression-history)))

(defun slime-bogus-completion-alist (list)
  "Make an alist out of list.
The same elements go in the CAR, and nil in the CDR. To support the
apparently very stupid `try-completions' interface, that wants an
alist but ignores CDRs."
  (mapcar (lambda (x) (cons x nil)) list))

(defun slime-simple-completions (prefix)
  (let ((slime-current-thread t))
    (slime-eval
     `(swank:simple-completions ,prefix ',(slime-current-package)))))


;;;; Edit definition

(defvar slime-find-definition-history-ring (make-ring 20)
  "History ring recording the definition-finding \"stack\".")

(defun slime-push-definition-stack-from-snapshot (emacs-snapshot)
  (with-struct (slime-emacs-snapshot. narrowing-configuration point-marker)
      emacs-snapshot
    (slime-push-definition-stack point-marker narrowing-configuration)))

(defun slime-push-definition-stack (&optional marker narrowing-configuration)
  "Add MARKER and NARROWING-CONFIGURATION to the edit-definition history stack.
If MARKER is nil, use the current point. If NARROWING-CONFIGURATION is nil, 
look if the current buffer is narrowed, and if so use the relevant values."
  (ring-insert-at-beginning slime-find-definition-history-ring 
    (list (or marker (point-marker))
          (or narrowing-configuration
              (slime-current-narrowing-configuration)))))

(defun slime-pop-find-definition-stack ()
  "Pop the edit-definition stack and goto the location."
  (interactive)
  (unless (ring-empty-p slime-find-definition-history-ring)
    (destructuring-bind (marker narrowing-cfg)
        (ring-remove slime-find-definition-history-ring)
      (let ((buffer (marker-buffer marker))
            (narrowedp  (slime-narrowing-configuration.narrowedp narrowing-cfg))
            (narrow-beg (slime-narrowing-configuration.beg narrowing-cfg))
            (narrow-end (slime-narrowing-configuration.end narrowing-cfg)))
        (if (buffer-live-p buffer)
            (progn (switch-to-buffer buffer)
                   (goto-char (marker-position marker))
                   (when narrowedp
                     (narrow-to-region narrow-beg narrow-end)))
            ;; If this buffer was deleted, recurse to try the next one
            (slime-pop-find-definition-stack))))))

(defstruct (slime-xref (:conc-name slime-xref.) (:type list))
  dspec location)

(defstruct (slime-location (:conc-name slime-location.) (:type list)
                           (:constructor nil)
                           (:copier nil))
  tag buffer position hints)

(defun slime-location-p (o) (and (consp o) (eq (car o) :location)))

(defun slime-xref-has-location-p (xref)
  (slime-location-p (slime-xref.location xref)))

(defun make-slime-buffer-location (buffer-name position &optional hints)
  `(:location (:buffer ,buffer-name) (:position ,position)
              ,(if hints `(:hints ,hints) `(:hints))))

(defun make-slime-file-location (file-name position &optional hints)
  `(:location (:file ,file-name) (:position ,position)
              ,(if hints `(:hints ,hints) `(:hints))))

;;; The hooks are tried in order until one succeeds, otherwise the
;;; default implementation involving `slime-find-definitions-function'
;;; is used. The hooks are called with the same arguments as
;;; `slime-edit-definition'.
(defvar slime-edit-definition-hooks)

(defun slime-edit-definition (name &optional where)
  "Lookup the definition of the name at point.  
If there's no name at point, or a prefix argument is given, then the
function name is prompted."
  (interactive (list (slime-read-symbol-name "Name: ")))
  (or (run-hook-with-args-until-success 'slime-edit-definition-hooks name where)
      (slime-edit-definition-cont (slime-find-definitions name)
                                  name where)))

(defun slime-edit-definition-cont (xrefs name where)
  (destructuring-bind (1loc file-alist) (slime-analyze-xrefs xrefs)
    (cond ((null xrefs) 
           (error "No known definition for: %s (in %s)" 
                  name (or (slime-current-package) (slime-lisp-package))))
          (1loc
           (slime-push-definition-stack)
           (slime-pop-to-location (slime-xref.location (car xrefs)) where))
          ((slime-length= xrefs 1)      ; ((:error "..."))
           (error "%s" (cadr (slime-xref.location (car xrefs)))))
          (t
           ;; Xref buffers will themselves push onto the find-definition stack.
           (slime-show-xrefs file-alist 'definition name
                             (slime-current-package))))))

(defun slime-analyze-xrefs (xrefs)
  "Find common filenames in XREFS.
Return a list (SINGLE-LOCATION FILE-ALIST).
SINGLE-LOCATION is true if all xrefs point to the same location.
FILE-ALIST is an alist of the form ((FILENAME . (XREF ...)) ...)."
  (list (and xrefs
             (let ((loc (slime-xref.location (car xrefs))))
               (and (slime-location-p loc)
                    (every (lambda (x) (equal (slime-xref.location x) loc))
                           (cdr xrefs)))))
        (slime-alistify xrefs #'slime-xref-group #'equal)))

(defun slime-xref-group (xref)
  (cond ((slime-xref-has-location-p xref)
         (destructure-case (slime-location.buffer (slime-xref.location xref))
           ((:file filename) filename)
           ((:buffer bufname)
            (let ((buffer (get-buffer bufname)))
              (if buffer 
                  (format "%S" buffer) ; "#<buffer foo.lisp>"
                (format "%s (previously existing buffer)" bufname))))
           ((:source-form _) "(S-Exp)")
           ((:zip zip entry) entry)))
        (t
         "(No location)")))

(defun slime-pop-to-location (location &optional where)
  (slime-goto-source-location location)
  (ecase where
    ((nil)     (switch-to-buffer (current-buffer)))
    (window    (pop-to-buffer (current-buffer) t))
    (frame     (let ((pop-up-frames t)) (pop-to-buffer (current-buffer) t)))
    (excursion nil))) ; NOP, slime-goto-source-location did set-buffer.

(defun slime-find-definitions (name)
  "Find definitions for NAME."
  (funcall slime-find-definitions-function name))

(defun slime-find-definitions-rpc (name)
  (slime-eval `(swank:find-definitions-for-emacs ,name)))
 
(defun slime-edit-definition-other-window (name)
  "Like `slime-edit-definition' but switch to the other window."
  (interactive (list (slime-read-symbol-name "Symbol: ")))
  (slime-edit-definition name 'window))

(defun slime-edit-definition-other-frame (name)
  "Like `slime-edit-definition' but switch to the other window."
  (interactive (list (slime-read-symbol-name "Symbol: ")))
  (slime-edit-definition name 'frame))

(defun slime-edit-definition-with-etags (name)
  (interactive (list (slime-read-symbol-name "Symbol: ")))
  (let ((xrefs (slime-etags-definitions name)))
    (cond (xrefs 
           (message "Using tag file...")
           (slime-edit-definition-cont xrefs name nil))
          (t
           (error "No known definition for: %s" name)))))

(defun slime-etags-definitions (name)
  "Search definitions matching NAME in the tags file.
The result is a (possibly empty) list of definitions."
  (require 'etags)
  (let ((defs '()))
    (save-excursion
      (let ((first-time t))
        (while (visit-tags-table-buffer (not first-time))
          (setq first-time nil)
          (goto-char (point-min))
          (while (search-forward name nil t)
            (beginning-of-line)
            (destructuring-bind (hint line &rest pos) (etags-snarf-tag)
              (unless (eq hint t)       ; hint==t if we are in a filename line
                (let ((file (expand-file-name (file-of-tag))))
                  (let ((loc `(:location (:file ,file)
                                         (:line ,line)
                                         (:snippet ,hint))))
                    (push (list hint loc) defs))))))))
      (reverse defs))))

;;;;; first-change-hook

(defun slime-first-change-hook ()
  "Notify Lisp that a source file's buffer has been modified."
  ;; Be careful not to disturb anything!
  ;; In particular if we muck up the match-data then query-replace
  ;; breaks. -luke (26/Jul/2004)
  (save-excursion
    (save-match-data
      (when (and (buffer-file-name)
                 (file-exists-p (buffer-file-name))
                 (slime-background-activities-enabled-p))
        (let ((filename (slime-to-lisp-filename (buffer-file-name))))          
           (slime-eval-async `(swank:buffer-first-change ,filename)))))))

(defun slime-setup-first-change-hook ()
  (add-hook (make-local-variable 'first-change-hook)
            'slime-first-change-hook))

(add-hook 'slime-mode-hook 'slime-setup-first-change-hook)


;;;; Eval for Lisp

(defun slime-eval-for-lisp (thread tag form-string)
  (let ((ok nil) 
        (value nil)
        (c (slime-connection)))
    (unwind-protect (progn
                      (slime-check-eval-in-emacs-enabled)
                      (setq value (eval (read form-string)))
                      (setq ok t))
      (let ((result (if ok `(:ok ,value) `(:abort))))
        (slime-dispatch-event `(:emacs-return ,thread ,tag ,result) c)))))

(defun slime-check-eval-in-emacs-enabled ()
  "Raise an error if `slime-enable-evaluate-in-emacs' isn't true."
  (unless slime-enable-evaluate-in-emacs
    (error (concat "slime-eval-in-emacs disabled for security."
                   "Set slime-enable-evaluate-in-emacs true to enable it."))))


;;;; `ED'

(defvar slime-ed-frame nil
  "The frame used by `slime-ed'.")

(defcustom slime-ed-use-dedicated-frame t
  "*When non-nil, `slime-ed' will create and reuse a dedicated frame."
  :type 'boolean
  :group 'slime-mode)

(defun slime-ed (what)
  "Edit WHAT.

WHAT can be:
  A filename (string),
  A list (FILENAME LINE [COLUMN]),
  A list (FILENAME :charpos CHARPOS),
  A function name (symbol or cons),
  nil.

This is for use in the implementation of COMMON-LISP:ED."
  ;; Without `save-excursion' very strange things happen if you call
  ;; (swank:ed-in-emacs X) from the REPL. -luke (18/Jan/2004)
  (save-excursion
    (when slime-ed-use-dedicated-frame
      (unless (and slime-ed-frame (frame-live-p slime-ed-frame))
        (setq slime-ed-frame (make-frame)))
      (select-frame slime-ed-frame))
    (cond ((stringp what)
           (find-file (slime-from-lisp-filename what)))
          ((and (consp what) (stringp (first what)))
           (find-file (first (slime-from-lisp-filename what)))
           (cond
            ((eql (second what) :charpos)
             (goto-char (third what)))
            (t
             (goto-line (second what))
             ;; Find the correct column, without going past the end of
             ;; the line.
             (let ((col (third what)))
               (while (and col
                           (< (point) (point-at-eol))
                           (/= (decf col) -1))
                 (forward-char 1))))))
          ((and what (symbolp what))
           (slime-edit-definition (symbol-name what)))
          ((consp what)
           (slime-edit-definition (prin1-to-string what)))
          (t nil))))                    ; nothing in particular


;;;; Interactive evaluation.

(defun slime-interactive-eval (string)
  "Read and evaluate STRING and print value in minibuffer.

Note: If a prefix argument is in effect then the result will be
inserted in the current buffer."
  (interactive (list (slime-read-from-minibuffer "Slime Eval: ")))
  (cond ((not current-prefix-arg)
         (slime-eval-with-transcript `(swank:interactive-eval ,string) 
                                     string))
        (t
         (slime-eval-print string))))

(defun slime-display-eval-result (value)
  (slime-message "%s" value))

(defun slime-eval-print (string)
  "Eval STRING in Lisp; insert any output and the result at point."
  (slime-eval-async `(swank:eval-and-grab-output ,string)
                    (lexical-let ((buffer (current-buffer)))
                      (lambda (result)
                        (with-current-buffer buffer
                          (destructuring-bind (output value) result
                            (insert output value)))))))

(defun slime-eval-with-transcript (form &optional msg no-popups cont)
  "Eval FROM in Lisp.  Display output, if any, caused by the evaluation."
  (when msg (slime-insert-transcript-delimiter msg))
  (setq slime-repl-popup-on-output (not no-popups))
  (setq cont (or cont #'slime-display-eval-result))
  (slime-rex (cont) (form)
    ((:ok value) (slime-eval-with-transcript-cont t value cont))
    ((:abort) (slime-eval-with-transcript-cont nil nil nil))))

(defun slime-insert-transcript-delimiter (string)
  (with-current-buffer (slime-output-buffer)
    (save-excursion
      (goto-char slime-repl-input-start-mark)
      (unless (bolp) (insert-before-markers "\n"))
      (slime-propertize-region '(slime-transcript-delimiter t)
        (insert-before-markers
         ";;;; " (subst-char-in-string ?\n ?\ 
                                       (substring string 0 
                                                  (min 60 (length string))))
         " ...\n"))
      (assert (= (point) slime-repl-input-start-mark))
      (slime-mark-output-start))
    (slime-repl-show-maximum-output)))

(defun slime-eval-with-transcript-cont (ok result cont)
  (run-with-timer 0.2 nil (lambda ()
                            (setq slime-repl-popup-on-output nil)))
  (with-current-buffer (slime-output-buffer)
    (save-excursion (slime-repl-insert-prompt))
    (slime-repl-show-maximum-output)
    (cond (ok (funcall cont result))
        (t (message "Evaluation aborted.")))))

(defun slime-eval-describe (form)
  "Evaluate FORM in Lisp and display the result in a new buffer."
  (slime-eval-async form (slime-rcurry #'slime-show-description
                                       (slime-current-package))))

(defun slime-show-description (string package)
  (slime-with-popup-buffer ("*SLIME Description*" package)
    (princ string)
    (goto-char (point-min))))

(defun slime-display-buffer-region (buffer start end &optional other-window)
  "Like `display-buffer', but only display the specified region."
  (let ((window-min-height 1))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (goto-char start)
          (beginning-of-line)
          (narrow-to-region (point) end)
          (let ((window (display-buffer buffer other-window t)))
            (set-window-start window (point))
            (unless (or (one-window-p t)
                        (/= (frame-width) (window-width)))
              (set-window-text-height window (/ (1- (frame-height)) 2)))
            (shrink-window-if-larger-than-buffer window)
            window))))))
  
(defun slime-last-expression ()
  (buffer-substring-no-properties
   (save-excursion (backward-sexp) (point))
   (point)))

(defun slime-eval-last-expression ()
  "Evaluate the expression preceding point."
  (interactive)
  (slime-interactive-eval (slime-last-expression)))

(defun slime-eval-last-expression-display-output ()
  "Display output buffer and evaluate the expression preceding point."
  (interactive)
  (slime-display-output-buffer)
  (slime-interactive-eval (slime-last-expression)))
  
(defun slime-eval-defun ()
  "Evaluate the current toplevel form.
Use `slime-re-evaluate-defvar' if the from starts with '(defvar'"
  (interactive)
  (let ((form (slime-defun-at-point)))
    (cond ((string-match "^(defvar " form)
           (slime-re-evaluate-defvar form))
          (t
           (slime-interactive-eval form)))))

(defun slime-eval-region (start end)
  "Evaluate region."
  (interactive "r")
  (slime-eval-with-transcript 
   `(swank:interactive-eval-region 
     ,(buffer-substring-no-properties start end))))

(defun slime-eval-buffer ()
  "Evaluate the current buffer.
The value is printed in the echo area."
  (interactive)
  (slime-eval-region (point-min) (point-max)))

(defun slime-re-evaluate-defvar (form)
  "Force the re-evaluaton of the defvar form before point.  

First make the variable unbound, then evaluate the entire form."
  (interactive (list (slime-last-expression)))
  (slime-eval-with-transcript `(swank:re-evaluate-defvar ,form)))

(defun slime-pprint-eval-last-expression ()
  "Evaluate the form before point; pprint the value in a buffer."
  (interactive)
  (slime-eval-describe `(swank:pprint-eval ,(slime-last-expression))))

(defun slime-eval-print-last-expression (string)
  "Evaluate sexp before point; print value into the current buffer"
  (interactive (list (slime-last-expression)))
  (insert "\n")
  (slime-eval-print string))

(defun slime-call-defun ()
  "Insert a call to the toplevel form defined around point into the REPL."
  (interactive)
  (flet ((insert-call (symbol)
           (let* ((qualified-symbol-name (slime-qualify-cl-symbol-name symbol))
                  (symbol-name (slime-cl-symbol-name qualified-symbol-name))
                  (symbol-package (slime-cl-symbol-package qualified-symbol-name))
                  (function-call 
                   (format "(%s " (if (equalp (slime-lisp-package) symbol-package)
                                      symbol-name
                                      qualified-symbol-name))))
             (slime-switch-to-output-buffer)
             (goto-char slime-repl-input-start-mark)
             (insert function-call)
             (save-excursion (insert ")")))))           
    (let ((toplevel (slime-parse-toplevel-form)))
      (if (symbolp toplevel)
          (error "Not in a function definition")
          (destructure-case toplevel
            (((:defun :defgeneric :defmacro :define-compiler-macro) symbol)
             (insert-call symbol))
            ((:defmethod symbol &rest args)
             (declare (ignore args))
             (insert-call symbol))
            (t
             (error "Not in a function definition")))))))

;;;; Edit Lisp value
;;;
(defun slime-edit-value (form-string)
  "\\<slime-edit-value-mode-map>\
Edit the value of a setf'able form in a new buffer.
The value is inserted into a temporary buffer for editing and then set
in Lisp when committed with \\[slime-edit-value-commit]."
  (interactive 
   (list (slime-read-from-minibuffer "Edit value (evaluated): "
				     (slime-sexp-at-point))))
  (slime-eval-async `(swank:value-for-editing ,form-string)
                    (lexical-let ((form-string form-string)
                                  (package (slime-current-package)))
                      (lambda (result)
                        (slime-edit-value-callback form-string result 
                                                   package)))))

(make-variable-buffer-local
 (defvar slime-edit-form-string nil
   "The form being edited by `slime-edit-value'."))

(define-minor-mode slime-edit-value-mode
  "Mode for editing a Lisp value."
  nil
  " Edit-Value"
  '(("\C-c\C-c" . slime-edit-value-commit)))

(defun slime-edit-value-callback (form-string current-value package)
  (let ((name (generate-new-buffer-name (format "*Edit %s*" form-string))))
    (with-current-buffer (slime-with-popup-buffer (name package t)
                           (current-buffer))
      (lisp-mode)
      (slime-mode 1)
      (slime-popup-buffer-mode -1)       ; don't want binding of 'q'
      (slime-edit-value-mode 1)
      (setq buffer-read-only nil)
      (setq slime-edit-form-string form-string)
      (insert current-value))))

(defun slime-edit-value-commit ()
  "Commit the edited value to the Lisp image.
\\(See `slime-edit-value'.)"
  (interactive)
  (if (null slime-edit-form-string)
      (error "Not editing a value.")
    (let ((value (buffer-substring-no-properties (point-min) (point-max))))
      (lexical-let ((buffer (current-buffer)))
        (slime-eval-async `(swank:commit-edited-value ,slime-edit-form-string
                                                      ,value)
                          (lambda (_)
                            (with-current-buffer buffer
                              (slime-popup-buffer-quit t))))))))

;;;; Tracing

(defun slime-redirect-trace-output ()
  "Redirect the trace output to a separate Emacs buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*SLIME Trace Output*")))
    (with-current-buffer buffer
      (let ((marker (copy-marker (buffer-size)))
            (target (incf slime-last-output-target-id)))
        (puthash target marker slime-output-target-to-marker)
        (slime-eval `(swank:redirect-trace-output ,target))))
    ;; Note: We would like the entries in
    ;; slime-output-target-to-marker to disappear when the buffers are
    ;; killed.  We cannot just make the hash-table ":weakness 'value"
    ;; -- there is no reference from the buffers to the markers in the
    ;; buffer, so entries would disappear even though the buffers are
    ;; alive.  Best solution might be to make buffer-local variables
    ;; that keep the markers. --mkoeppe
    (pop-to-buffer buffer)))

(defun slime-untrace-all ()
  "Untrace all functions."
  (interactive)
  (slime-eval `(swank:untrace-all)))

(defun slime-toggle-trace-fdefinition (&optional using-context-p)
  "Toggle trace."
  (interactive "P")
  (let* ((spec (if using-context-p
                  (slime-extract-context)
                 (slime-symbol-name-at-point)))
         (spec (slime-trace-query spec)))
    (message "%s" (slime-eval `(swank:swank-toggle-trace ,spec)))))

(defun slime-trace-query (spec)
  "Ask the user which function to trace; SPEC is the default.
The result is a string."
  (cond ((null spec)
         (slime-read-from-minibuffer "(Un)trace: "))
        ((stringp spec)
         (slime-read-from-minibuffer "(Un)trace: " spec))
        (t
         (destructure-case spec
           ((setf n)
            (slime-read-from-minibuffer "(Un)trace: " (prin1-to-string spec)))
           ((:defun n)
            (slime-read-from-minibuffer "(Un)trace: " (prin1-to-string n)))
           ((:defgeneric n)
            (let* ((name (prin1-to-string n))
                   (answer (slime-read-from-minibuffer "(Un)trace: " name)))
              (cond ((and (string= name answer)
                          (y-or-n-p (concat "(Un)trace also all " 
                                            "methods implementing " 
                                            name "? ")))
                     (prin1-to-string `(:defgeneric ,n)))
                    (t
                     answer))))
           ((:defmethod &rest _)
            (slime-read-from-minibuffer "(Un)trace: " (prin1-to-string spec)))
           ((:call caller callee)
            (let* ((callerstr (prin1-to-string caller))
                   (calleestr (prin1-to-string callee))
                   (answer (slime-read-from-minibuffer "(Un)trace: " 
                                                       calleestr)))
              (cond ((and (string= calleestr answer)
                          (y-or-n-p (concat "(Un)trace only when " calleestr
                                            " is called by " callerstr "? ")))
                     (prin1-to-string `(:call ,caller ,callee)))
                    (t
                     answer))))
           (((:labels :flet) &rest _)
            (slime-read-from-minibuffer "(Un)trace local function: "
                                        (prin1-to-string spec)))
           (t (error "Don't know how to trace the spec %S" spec))))))

(defun slime-extract-context ()
  "Parse the context for the symbol at point.  
Nil is returned if there's no symbol at point.  Otherwise we detect
the following cases (the . shows the point position):

 (defun n.ame (...) ...)                 -> (:defun name)
 (defun (setf n.ame) (...) ...)          -> (:defun (setf name))
 (defmethod n.ame (...) ...)             -> (:defmethod name (...))
 (defun ... (...) (labels ((n.ame (...)  -> (:labels (:defun ...) name)
 (defun ... (...) (flet ((n.ame (...)    -> (:flet (:defun ...) name)
 (defun ... (...) ... (n.ame ...) ...)   -> (:call (:defun ...) name)
 (defun ... (...) ... (setf (n.ame ...)  -> (:call (:defun ...) (setf name))

 (defmacro n.ame (...) ...)              -> (:defmacro name)
 (defsetf n.ame (...) ...)               -> (:defsetf name)
 (define-setf-expander n.ame (...) ...)  -> (:define-setf-expander name)
 (define-modify-macro n.ame (...) ...)   -> (:define-modify-macro name)
 (define-compiler-macro n.ame (...) ...) -> (:define-compiler-macro name)

For other contexts we return the symbol at point."
  (let ((name (slime-symbol-name-at-point)))
    (if name
        (let ((symbol (read name)))
          (or (progn ;;ignore-errors 
                (slime-parse-context symbol))
              symbol)))))

(defun slime-parse-context (name)
  (save-excursion 
    (cond ((slime-in-expression-p '(defun *))          `(:defun ,name))
          ((slime-in-expression-p '(defmacro *))       `(:defmacro ,name))
          ((slime-in-expression-p '(defgeneric *))     `(:defgeneric ,name))
          ((slime-in-expression-p '(setf *))
           ;;a setf-definition, but which?
           (backward-up-list 1)
           (slime-parse-context `(setf ,name)))
          ((slime-in-expression-p '(defmethod *))
           (unless (looking-at "\\s ")
             (forward-sexp 1)) ; skip over the methodname
           (let (qualifiers arglist)
             (loop for e = (read (current-buffer))
                   until (listp e) do (push e qualifiers)
                   finally (setq arglist e))
             `(:defmethod ,name ,@qualifiers
                          ,(slime-arglist-specializers arglist))))
          ((and (symbolp name) 
                (slime-in-expression-p `(,name)))
           ;; looks like a regular call
           (let ((toplevel (ignore-errors (slime-parse-toplevel-form))))
             (cond ((slime-in-expression-p `(setf (*)))  ;a setf-call
                    (if toplevel
                        `(:call ,toplevel (setf ,name))
                      `(setf ,name)))
                   ((not toplevel)
                    name)
                   ((slime-in-expression-p `(labels ((*))))
                    `(:labels ,toplevel ,name))
                   ((slime-in-expression-p `(flet ((*))))
                    `(:flet ,toplevel ,name))
                   (t
                    `(:call ,toplevel ,name)))))
          ((slime-in-expression-p '(define-compiler-macro *))
           `(:define-compiler-macro ,name))
          ((slime-in-expression-p '(define-modify-macro *))
           `(:define-modify-macro ,name))
          ((slime-in-expression-p '(define-setf-expander *))
           `(:define-setf-expander ,name))
          ((slime-in-expression-p '(defsetf *))
           `(:defsetf ,name))
          (t 
           name))))

(defun slime-at-list-p (&optional skip-blanks)
  (save-excursion
    (when skip-blanks 
      (slime-forward-blanks))
    (ignore-errors 
      (= (point) (progn (down-list 1) (backward-up-list 1) (point))))))

(defun slime-at-expression-p (pattern &optional skip-blanks)
  (when (slime-at-list-p skip-blanks)
    (save-excursion
      (down-list 1)
      (slime-in-expression-p pattern))))

(defun slime-in-expression-p (pattern)
  "A helper function to determine the current context.
The pattern can have the form:
 pattern ::= ()    ;matches always
           | (*)   ;matches inside a list
           | (<symbol> <pattern>)   ;matches if the first element in
				    ; the current list is <symbol> and
                                    ; if <pattern> matches.
           | ((<pattern>))          ;matches if we are in a nested list."
  (save-excursion
    (let ((path (reverse (slime-pattern-path pattern))))
      (loop for p in path
            always (ignore-errors 
                     (etypecase p
                       (symbol (slime-beginning-of-list) 
                               (eq (read (current-buffer)) p))
                       (number (backward-up-list p)
                               t)))))))

(defun slime-pattern-path (pattern)
  ;; Compute the path to the * in the pattern to make matching
  ;; easier. The path is a list of symbols and numbers.  A number
  ;; means "(down-list <n>)" and a symbol "(look-at <sym>)")
  (if (null pattern)
      '()
    (etypecase (car pattern)
      ((member *) '())
      (symbol (cons (car pattern) (slime-pattern-path (cdr pattern))))
      (cons (cons 1 (slime-pattern-path (car pattern)))))))

(defun slime-beginning-of-list (&optional up)
  "Move backward to the beginning of the current expression.
Point is placed before the first expression in the list."
  (backward-up-list (or up 1))
  (down-list 1)
  (skip-syntax-forward " "))

(defun slime-end-of-list (&optional up)
  (backward-up-list (or up 1))
  (forward-list 1)
  (down-list -1))

(defun slime-parse-toplevel-form ()
  (ignore-errors                        ; (foo)
    (save-excursion
      (beginning-of-defun)
      (down-list 1)
      (forward-sexp 1)
      (slime-parse-context (read (current-buffer))))))
		 
(defun slime-arglist-specializers (arglist)
  (cond ((or (null arglist)
	     (member (first arglist) '(&optional &key &rest &aux)))
	 (list))
	((consp (first arglist))
	 (cons (second (first arglist))
	       (slime-arglist-specializers (rest arglist))))
	(t
	 (cons 't 
	       (slime-arglist-specializers (rest arglist))))))

(defun slime-disassemble-symbol (symbol-name)
  "Display the disassembly for SYMBOL-NAME."
  (interactive (list (slime-read-symbol-name "Disassemble: ")))
  (slime-eval-describe `(swank:disassemble-symbol ,symbol-name)))

(defun slime-undefine-function (symbol-name)
  "Unbind the function slot of SYMBOL-NAME."
  (interactive (list (slime-read-symbol-name "fmakunbound: " t)))
  (slime-eval-async `(swank:undefine-function ,symbol-name)
                    (lambda (result) (message "%s" result))))

(defun slime-load-file (filename)
  "Load the Lisp file FILENAME."
  (interactive (list 
		(read-file-name "Load file: " nil nil
				nil (if (buffer-file-name)
                                        (file-name-nondirectory 
                                         (buffer-file-name))))))
  (let ((lisp-filename (slime-to-lisp-filename (expand-file-name filename))))
    (slime-eval-with-transcript `(swank:load-file ,lisp-filename))))


;;;; Profiling

(defun slime-toggle-profile-fdefinition (fname-string)
  "Toggle profiling for FNAME-STRING."
  (interactive (list (slime-read-from-minibuffer 
                      "(Un)Profile: "
                      (slime-symbol-name-at-point))))
  (slime-eval-async `(swank:toggle-profile-fdefinition ,fname-string)
                    (lambda (r) (message "%s" r))))

(defun slime-unprofile-all ()
  "Unprofile all functions."
  (interactive)
  (slime-eval-async '(swank:unprofile-all)
                    (lambda (r) (message "%s" r))))

(defun slime-profile-report ()
  "Print profile report."
  (interactive)
  (slime-eval-with-transcript '(swank:profile-report)))

(defun slime-profile-reset ()
  "Reset profile counters."
  (interactive)
  (slime-eval-async (slime-eval `(swank:profile-reset))
                    (lambda (r) (message "%s" r))))

(defun slime-profiled-functions ()
  "Return list of names of currently profiled functions."
  (interactive)
  (slime-eval-async `(swank:profiled-functions)
                    (lambda (r) (message "%s" r))))

(defun slime-profile-package (package callers methods)
  "Profile all functions in PACKAGE.  
If CALLER is non-nil names have counts of the most common calling
functions recorded. 
If METHODS is non-nil, profile all methods of all generic function
having names in the given package."
  (interactive (list (slime-read-package-name "Package: ")
                     (y-or-n-p "Record the most common callers? ")
                     (y-or-n-p "Profile methods? ")))
  (slime-eval-async `(swank:profile-package ,package ,callers ,methods)
                    (lambda (r) (message "%s" r))))



;;;; Documentation

(defun slime-hyperspec-lookup (symbol-name)
  "A wrapper for `hyperspec-lookup'"
  (interactive (list (let* ((symbol-at-point (slime-symbol-name-at-point))
                            (stripped-symbol 
                             (and symbol-at-point
                                  (downcase
                                   (common-lisp-hyperspec-strip-cl-package 
                                    symbol-at-point)))))
                       (if (and stripped-symbol
                                (intern-soft stripped-symbol
                                             common-lisp-hyperspec-symbols))
                           stripped-symbol
                         (completing-read
                          "Look up symbol in Common Lisp HyperSpec: "
                          common-lisp-hyperspec-symbols #'boundp
                          t stripped-symbol
                          'common-lisp-hyperspec-history)))))
  (hyperspec-lookup symbol-name))
  
(defun slime-describe-symbol (symbol-name)
  "Describe the symbol at point."
  (interactive (list (slime-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (slime-eval-describe `(swank:describe-symbol ,symbol-name)))

(defun slime-documentation (symbol-name)
  "Display function- or symbol-documentation for SYMBOL-NAME."
  (interactive (list (slime-read-symbol-name "Documentation for symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (slime-eval-describe 
   `(swank:documentation-symbol ,symbol-name "(not documented)")))

(defun slime-describe-function (symbol-name)
  (interactive (list (slime-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (slime-eval-describe `(swank:describe-function ,symbol-name)))

(defun slime-apropos-summary (string case-sensitive-p package only-external-p)
  "Return a short description for the performed apropos search."
  (concat (if case-sensitive-p "Case-sensitive " "")
          "Apropos for "
          (format "%S" string)
          (if package (format " in package %S" package) "")
          (if only-external-p " (external symbols only)" "")))

(defun slime-apropos (string &optional only-external-p package 
                             case-sensitive-p)
  "Show all bound symbols whose names match STRING. With prefix
arg, you're interactively asked for parameters of the search."
  (interactive
   (if current-prefix-arg
       (list (read-string "SLIME Apropos: ")
             (y-or-n-p "External symbols only? ")
             (let ((pkg (slime-read-package-name "Package: ")))
               (if (string= pkg "") nil pkg))
             (y-or-n-p "Case-sensitive? "))
     (list (read-string "SLIME Apropos: ") t nil nil)))
  (let ((buffer-package (or package (slime-current-package))))
    (slime-eval-async
     `(swank:apropos-list-for-emacs ,string ,only-external-p
                                    ,case-sensitive-p ',package)
     (slime-rcurry #'slime-show-apropos string buffer-package
                   (slime-apropos-summary string case-sensitive-p
                                          package only-external-p)))))

(defun slime-apropos-all ()
  "Shortcut for (slime-apropos <string> nil nil)"
  (interactive)
  (slime-apropos (read-string "SLIME Apropos: ") nil nil))

(defun slime-apropos-package (package &optional internal)
  "Show apropos listing for symbols in PACKAGE.
With prefix argument include internal symbols."
  (interactive (list (let ((pkg (slime-read-package-name "Package: ")))
                       (if (string= pkg "") (slime-current-package) pkg))
                     current-prefix-arg))
  (slime-apropos "" (not internal) package))

(defun slime-show-apropos (plists string package summary)
  (if (null plists)
      (message "No apropos matches for %S" string)
    (slime-with-popup-buffer ("*SLIME Apropos*" package t)
      (apropos-mode)
      (if (boundp 'header-line-format)
          (setq header-line-format summary)
        (insert summary "\n\n"))
      (slime-set-truncate-lines)
      (slime-print-apropos plists)
      (set-syntax-table lisp-mode-syntax-table)
      (goto-char (point-min)))))

(eval-when-compile (require 'apropos))

(defvar slime-apropos-label-properties
  (progn
    (require 'apropos)
    (cond ((and (boundp 'apropos-label-properties) 
                (symbol-value 'apropos-label-properties)))
          ((boundp 'apropos-label-face)
           (etypecase (symbol-value 'apropos-label-face)
             (symbol `(face ,(or (symbol-value 'apropos-label-face)
                                 'italic)
                            mouse-face highlight))
             (list (symbol-value 'apropos-label-face)))))))

(defun slime-print-apropos (plists)
  (dolist (plist plists)
    (let ((designator (plist-get plist :designator)))
      (assert designator)
      (slime-insert-propertized `(face ,apropos-symbol-face) designator))
    (terpri)
    (let ((apropos-label-properties slime-apropos-label-properties))
      (loop for (prop namespace) 
	    in '((:variable "Variable")
		 (:function "Function")
		 (:generic-function "Generic Function")
                 (:macro "Macro")
                 (:special-operator "Special Operator")
		 (:setf "Setf")
		 (:type "Type")
		 (:class "Class")
                 (:alien-type "Alien type")
                 (:alien-struct "Alien struct")
                 (:alien-union "Alien type")
                 (:alien-enum "Alien enum"))
            ;; Properties not listed here will not show up in the buffer
	    do
	    (let ((value (plist-get plist prop))
		  (start (point)))
	      (when value
		(princ "  ") 
		(slime-insert-propertized apropos-label-properties namespace)
		(princ ": ")
		(princ (etypecase value
			 (string value)
			 ((member :not-documented) "(not documented)")))
                (add-text-properties 
                 start (point)
                 (list 'type prop 'action 'slime-call-describer
                       'button t 'apropos-label namespace 
                       'item (plist-get plist :designator)))
		(terpri)))))))

(defun slime-call-describer (arg)
  (let* ((pos (if (markerp arg) arg (point)))
         (type (get-text-property pos 'type))
         (item (get-text-property pos 'item)))
    (slime-eval-describe `(swank:describe-definition-for-emacs ,item ,type))))


;;;; XREF: cross-referencing

(defvar slime-xref-mode-map)
(defvar slime-xref-saved-emacs-snapshot nil
  "Buffer local variable in xref windows.")

(define-derived-mode slime-xref-mode lisp-mode "Xref"
  "slime-xref-mode: Major mode for cross-referencing.
\\<slime-xref-mode-map>\
The most important commands:
\\[slime-xref-quit]	- Dismiss buffer.
\\[slime-show-xref]	- Display referenced source and keep xref window.
\\[slime-goto-xref]	- Jump to referenced source and dismiss xref window.

\\{slime-xref-mode-map}
\\{slime-popup-buffer-mode-map}
"
  (setq font-lock-defaults nil)
  (setq delayed-mode-hooks nil)
  (slime-mode -1))

(slime-define-keys slime-xref-mode-map 
  ((kbd "RET") 'slime-show-xref)
  ([return] 'slime-show-xref)
  ("\C-m" 'slime-show-xref)
  (" " 'slime-goto-xref)
  ("n" 'slime-next-line/not-add-newlines)
  ("p" 'previous-line)
  ("\C-c\C-c" 'slime-recompile-xref)
  ("\C-c\C-k" 'slime-recompile-all-xrefs)
  ("\M-," 'slime-xref-retract))

(defun slime-next-line/not-add-newlines ()
  (interactive)
  (let ((next-line-add-newlines nil))
    (next-line 1)))


;;;;; XREF results buffer and window management

(defmacro* slime-with-xref-buffer ((xref-type symbol &optional package emacs-snapshot)  
                                   &body body)
  "Execute BODY in a xref buffer, then show that buffer."
  `(let ((xref-buffer-name% (format "*XREF[%s: %s]*" ,xref-type ,symbol)))
     (slime-with-popup-buffer (xref-buffer-name% ,package t ,emacs-snapshot)
       (slime-xref-mode)
       (slime-set-truncate-lines)
       (setq slime-popup-buffer-quit-function 'slime-xref-quit)
       (erase-buffer)
       (prog1 (progn ,@body)
         (assert (equal (buffer-name) xref-buffer-name%))
         (shrink-window-if-larger-than-buffer)))))

(put 'slime-with-xref-buffer 'lisp-indent-function 1)

(defun slime-xref-buffer ()
  "Return the XREF results buffer.
If CREATE is non-nil, create it if necessary."
  (or (find-if (lambda (b) (string-match "*XREF\\[" (buffer-name b)))
               (buffer-list))
      (error "No XREF buffer")))

(defun slime-xref-quit (&optional _)
  "Kill the current xref buffer, restore the window configuration
if appropriate."
  (interactive)
  ;; We can't simply use `slime-popup-buffer-quit' because we also
  ;; want the Xref window be deleted.
  (if (slime-popup-buffer-snapshot-unchanged-p)
      (slime-xref-retract)
    (let ((snapshot slime-popup-buffer-saved-emacs-snapshot)
          (buffer   (current-buffer)))
      ;; Make M-, work after Xref'ing.
      (slime-push-definition-stack-from-snapshot snapshot)
      (delete-windows-on buffer)
      (kill-buffer buffer))))

(defun slime-xref-retract ()
  "Leave the Xref buffer, and make everything as of before."
  (interactive)
  (let ((buffer (current-buffer)))
    (slime-popup-buffer-restore-snapshot)
    (kill-buffer buffer)))

(defun slime-insert-xrefs (xref-alist)
  "Insert XREF-ALIST in the current-buffer.
XREF-ALIST is of the form ((GROUP . ((LABEL LOCATION) ...)) ...).
GROUP and LABEL are for decoration purposes.  LOCATION is a
source-location."
  (loop for (group . refs) in xref-alist do 
        (slime-insert-propertized '(face bold) group "\n")
        (loop for (label location) in refs do
              (slime-insert-propertized (list 'slime-location location
                                              'face 'font-lock-keyword-face)
                                        "  " (slime-one-line-ify label) "\n")))
  ;; Remove the final newline to prevent accidental window-scrolling
  (backward-delete-char 1))

(defvar slime-next-location-function nil
  "Function to call for going to the next location.")

(defun slime-show-xrefs (xrefs type symbol package &optional emacs-snapshot)
  "Show the results of an XREF query."
  (if (null xrefs)
      (message "No references found for %s." symbol)
    (setq slime-next-location-function 'slime-goto-next-xref)
    (slime-with-xref-buffer (type symbol package emacs-snapshot)
      (slime-insert-xrefs xrefs)
      (goto-char (point-min))
      (forward-line)
      (skip-chars-forward " \t"))))


;;;;; XREF commands

(defun slime-who-calls (symbol)
  "Show all known callers of the function SYMBOL."
  (interactive (list (slime-read-symbol-name "Who calls: " t)))
  (slime-xref :calls symbol))

(defun slime-calls-who (symbol)
  "Show all known functions called by the function SYMBOL."
  (interactive (list (slime-read-symbol-name "Who calls: " t)))
  (slime-xref :calls-who symbol))

(defun slime-who-references (symbol)
  "Show all known referrers of the global variable SYMBOL."
  (interactive (list (slime-read-symbol-name "Who references: " t)))
  (slime-xref :references symbol))

(defun slime-who-binds (symbol)
  "Show all known binders of the global variable SYMBOL."
  (interactive (list (slime-read-symbol-name "Who binds: " t)))
  (slime-xref :binds symbol))

(defun slime-who-sets (symbol)
  "Show all known setters of the global variable SYMBOL."
  (interactive (list (slime-read-symbol-name "Who sets: " t)))
  (slime-xref :sets symbol))

(defun slime-who-macroexpands (symbol)
  "Show all known expanders of the macro SYMBOL."
  (interactive (list (slime-read-symbol-name "Who macroexpands: " t)))
  (slime-xref :macroexpands symbol))

(defun slime-who-specializes (symbol)
  "Show all known methods specialized on class SYMBOL."
  (interactive (list (slime-read-symbol-name "Who specializes: " t)))
  (slime-xref :specializes symbol))

(defun slime-list-callers (symbol-name)
  "List the callers of SYMBOL-NAME in a xref window."
  (interactive (list (slime-read-symbol-name "List callers: ")))
  (slime-xref :callers symbol-name))

(defun slime-list-callees (symbol-name)
  "List the callees of SYMBOL-NAME in a xref window."
  (interactive (list (slime-read-symbol-name "List callees: ")))
  (slime-xref :callees symbol-name))

(defun slime-xref (type symbol)
  "Make an XREF request to Lisp."
  (slime-eval-async
   `(swank:xref ',type ',symbol)
   (lexical-let ((type type)
                 (symbol symbol)
                 (package (slime-current-package))
                 (snapshot (slime-current-emacs-snapshot)))
     (lambda (result)
       (let ((file-alist (cadr (slime-analyze-xrefs result))))
         (slime-show-xrefs file-alist type symbol package snapshot))))))


;;;;; XREF navigation

(defun slime-xref-location-at-point ()
  (save-excursion
    ;; When the end of the last line is at (point-max) we can't find
    ;; the text property there. Going to bol avoids this problem.
    (beginning-of-line 1)
    (or (get-text-property (point) 'slime-location)
        (error "No reference at point."))))

(defun slime-xref-dspec-at-point ()
  (save-excursion
    (beginning-of-line 1)
    (with-syntax-table lisp-mode-syntax-table
      (forward-sexp)                    ; skip initial whitespaces
      (backward-sexp)
      (slime-sexp-at-point))))

(defun slime-all-xrefs ()
  (let ((xrefs nil))
    (save-excursion
      (goto-char (point-min))
      (while (ignore-errors (slime-next-line/not-add-newlines) t)
        (when-let (loc (get-text-property (point) 'slime-location))
          (let* ((dspec (slime-xref-dspec-at-point))
                 (xref  (make-slime-xref :dspec dspec :location loc)))
            (push xref xrefs)))))
    (nreverse xrefs)))

(defun slime-goto-xref ()
  "Goto the cross-referenced location at point."
  (interactive)
  ;; Notice: We implement it this way so `slime-show-xref' changes the
  ;; the window snapshot such that `slime-xref-quit' will push onto
  ;; the find-definition-stack.
  (slime-show-xref)
  (slime-xref-quit))

(defun slime-show-xref ()
  "Display the xref at point in the other window."
  (interactive)
  (let ((location (slime-xref-location-at-point)))
    (slime-show-source-location location)))
      
(defun slime-goto-next-xref ()
  "Goto the next cross-reference location."
  (let ((location (with-current-buffer (slime-xref-buffer)
                    (let ((w (display-buffer (current-buffer) t)))
                      (goto-char (1+ (next-single-char-property-change 
                                      (point) 'slime-location)))
                      (set-window-point w (point)))
                    (cond ((eobp)
                           (message "No more xrefs.")
                           nil)
                          (t 
                           (slime-xref-location-at-point))))))
    (when location
      (slime-pop-to-location location))))

(defun slime-next-location ()
  "Go to the next location, depending on context.
When displaying XREF information, this goes to the next reference."
  (interactive)
  (when (null slime-next-location-function)
    (error "No context for finding locations."))
  (funcall slime-next-location-function))

(defun slime-recompile-xref (&optional raw-prefix-arg)
  (interactive "P")
  (let* ((prefix-arg (and raw-prefix-arg (prefix-numeric-value raw-prefix-arg)))
         (debug-level (slime-normalize-optimization-level prefix-arg)))
    (let ((location (slime-xref-location-at-point))
          (dspec    (slime-xref-dspec-at-point)))
      (add-hook 'slime-compilation-finished-hook 
                (slime-make-xref-recompilation-cont (list dspec))
                nil)
      (slime-recompile-location location debug-level))))

(defun slime-recompile-all-xrefs (&optional raw-prefix-arg)
  (interactive "P")
  (let* ((prefix-arg (and raw-prefix-arg (prefix-numeric-value raw-prefix-arg)))
         (debug-level (slime-normalize-optimization-level prefix-arg)))
    (let ((dspecs) (locations))
      (dolist (xref (slime-all-xrefs))
        (when (slime-xref-has-location-p xref)
          (push (slime-xref.dspec xref) dspecs)
          (push (slime-xref.location xref) locations)))
      (add-hook 'slime-compilation-finished-hook 
                (slime-make-xref-recompilation-cont dspecs)
                nil)
      (slime-recompile-locations locations debug-level))))

(defun slime-make-xref-recompilation-cont (dspecs)
  ;; Extreme long-windedness to insert status of recompilation;
  ;; sometimes Elisp resembles more of an Ewwlisp.
  (lexical-let ((dspecs dspecs) (buffer (current-buffer)))
    (labels ((recompilation-cont (&rest args)
               (with-current-buffer buffer
                 (remove-hook 'slime-compilation-finished-hook
                              #'recompilation-cont)
                 (save-excursion
                   (slime-xref-insert-recompilation-flags 
                    dspecs (slime-compiler-results))))))
      #'recompilation-cont)))

(defun slime-xref-insert-recompilation-flags (dspecs compilation-results)
  (let* ((buffer-read-only nil)
         (max-column (slime-column-max)))
    (goto-char (point-min))
    (loop for dspec in dspecs
          for result in compilation-results
          do (save-excursion
               (loop for dspec-at-point = (progn (search-forward dspec)
                                                 (slime-xref-dspec-at-point))
                     until (equal dspec-at-point dspec))
               (end-of-line) ; skip old status information.
               (dotimes (i (- max-column (current-column)))
                 (insert " "))
               (insert " ")
               (insert (format "[%s]"
                               (case result
                                 ((t)   :success)
                                 ((nil) :failure)
                                 (t     result))))))))


;;;; Macroexpansion

(define-minor-mode slime-macroexpansion-minor-mode
    "SLIME mode for macroexpansion"
    nil
  " Macroexpand"
  '(("g" . slime-macroexpand-again)))

(flet ((remap (from to)
         (dolist (mapping (where-is-internal from slime-mode-map))
           (define-key slime-macroexpansion-minor-mode-map mapping to))))
  (remap 'slime-macroexpand-1 'slime-macroexpand-1-inplace)
  (remap 'slime-macroexpand-all 'slime-macroexpand-all-inplace)
  (remap 'advertised-undo 
         '(lambda (&optional arg)
            (interactive)
            (let ((inhibit-read-only t))
              (when (fboundp 'slime-remove-edits)
                (slime-remove-edits (point-min) (point-max)))
              (undo arg)))))

(defun slime-sexp-at-point-for-macroexpansion ()
  "Essentially like SLIME-SEXP-AT-POINT-OR-ERROR, but behaves a
bit more sanely in situations like ,(loop ...) where you want to
expand the LOOP form. See comment in the source of this function."
  (let ((string (slime-sexp-at-point-or-error))
        (bounds (bounds-of-thing-at-point 'sexp))
        (char-at-point (substring-no-properties (thing-at-point 'char))))
    ;; SLIME-SEXP-AT-POINT(-OR-ERROR) uses (THING-AT-POINT 'SEXP)
    ;; which is quite a bit botched: it returns "'(FOO BAR BAZ)" even
    ;; when point is placed _at the opening parenthesis_, and hence
    ;; "(FOO BAR BAZ)" wouldn't get expanded. Likewise for ",(...)",
    ;; ",@(...)" (would return "@(...)"!!), and "\"(...)".
    ;; So we better fix this up here:
    (when (string= char-at-point "(")
      (let ((char0 (elt string 0)))
        (when (member char0 '(?\' ?\, ?\" ?\@))
          (setf string (substring string 1))
          (incf (car bounds)))))
    (list string bounds)))

(defvar slime-eval-macroexpand-expression nil
  "Specifies the last macroexpansion preformed. 
This variable specifies both what was expanded and how.")

(defun slime-eval-macroexpand (expander &optional string)
  (let ((string (or string
                    (car (slime-sexp-at-point-for-macroexpansion)))))
    (setq slime-eval-macroexpand-expression `(,expander ,string))
    (slime-eval-async slime-eval-macroexpand-expression 
                      (slime-rcurry #'slime-show-macroexpansion 
                                    (slime-create-macroexpansion-buffer)))))

(defun slime-show-macroexpansion (expansion buffer)
  (pop-to-buffer buffer)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert expansion)
    (goto-char (point-min))
    (indent-sexp)
    (font-lock-fontify-buffer)))

(defun slime-create-macroexpansion-buffer ()
  (let ((name "*SLIME Macroexpansion*"))
    (slime-with-popup-buffer (name t t)
      (lisp-mode)
      (slime-mode 1)
      (slime-macroexpansion-minor-mode 1)
      (setq font-lock-keywords-case-fold-search t)
      (current-buffer))))

(defun slime-eval-macroexpand-inplace (expander)
  "Substitutes the current sexp at place with its macroexpansion.

NB: Does not affect *slime-eval-macroexpand-expression*"
  (interactive)
  (destructuring-bind (string bounds)
      (slime-sexp-at-point-for-macroexpansion)
    (lexical-let* ((start (car bounds))
                   (end (cdr bounds))
                   (point (point))
                   (package (slime-current-package))
                   (buffer (current-buffer)))
      (slime-eval-async 
       `(,expander ,string)
       (lambda (expansion)
         (with-current-buffer buffer
           (let ((buffer-read-only nil))
             (when (fboundp 'slime-remove-edits)
               (slime-remove-edits (point-min) (point-max)))
             (goto-char start)
             (delete-region start end)
             (insert expansion)
             (goto-char start)
             (indent-sexp)
             (goto-char point))))))))

(defun slime-macroexpand-1 (&optional repeatedly)
  "Display the macro expansion of the form at point.  The form is
expanded with CL:MACROEXPAND-1 or, if a prefix argument is given, with
CL:MACROEXPAND."
  (interactive "P")
  (slime-eval-macroexpand
   (if repeatedly 'swank:swank-macroexpand 'swank:swank-macroexpand-1)))

(defun slime-macroexpand-1-inplace (&optional repeatedly)
  (interactive "P")
  (slime-eval-macroexpand-inplace
   (if repeatedly 'swank:swank-macroexpand 'swank:swank-macroexpand-1)))

(defun slime-macroexpand-all ()
  "Display the recursively macro expanded sexp at point."
  (interactive)
  (slime-eval-macroexpand 'swank:swank-macroexpand-all))

(defun slime-macroexpand-all-inplace ()
  "Display the recursively macro expanded sexp at point."
  (interactive)
  (slime-eval-macroexpand-inplace 'swank:swank-macroexpand-all))

(defun slime-compiler-macroexpand ()
  "Display the compiler-macro expansion of sexp at point."
  (interactive)
  (slime-eval-macroexpand 'swank:swank-compiler-macroexpand))

(defun slime-compiler-macroexpand-1 ()
  "Display the compiler-macro expansion of sexp at point."
  (interactive)
  (slime-eval-macroexpand 'swank:swank-compiler-macroexpand-1))

(defun slime-macroexpand-again ()
  "Reperform the last macroexpansion."
  (interactive)
  (slime-eval-async slime-eval-macroexpand-expression 
                    (slime-rcurry #'slime-show-macroexpansion
                                  (current-buffer))))


;;;; Subprocess control

(defun slime-interrupt ()
  "Interrupt Lisp."
  (interactive)
  (cond ((slime-use-sigint-for-interrupt) (slime-send-sigint))
        (t (slime-dispatch-event `(:emacs-interrupt ,slime-current-thread)))))

(defun slime-quit ()
  (error "Not implemented properly.  Use `slime-interrupt' instead."))

(defun slime-quit-lisp (&optional kill)
  "Quit lisp, kill the inferior process and associated buffers."
  (interactive "P")
  (slime-quit-lisp-internal (slime-connection) 'slime-quit-sentinel kill))

(defun slime-quit-lisp-internal (connection sentinel kill)
  (let ((slime-dispatching-connection connection))
    (slime-eval-async '(swank:quit-lisp))
    (let* ((process (slime-inferior-process connection)))
      (kill-buffer (slime-output-buffer))
      (set-process-filter connection  nil)
      (set-process-sentinel connection sentinel)
      (when (and kill process)
        (sleep-for 0.2)
        (kill-process process)))))

(defun slime-quit-sentinel (process message)
  (assert (process-status process) 'closed)
  (let* ((inferior (slime-inferior-process process))
         (inferior-buffer (if inferior (process-buffer inferior))))
    (when inferior (delete-process inferior))
    (when inferior-buffer (kill-buffer inferior-buffer))
    (slime-net-close process)
    (message "Connection closed.")))

(defun slime-set-default-directory (directory)
  "Make DIRECTORY become Lisp's current directory."
  (interactive (list (read-directory-name "Directory: " nil nil t)))
  (let ((dir (expand-file-name directory)))
    (message "default-directory: %s"
             (slime-from-lisp-filename
              (slime-repl-shortcut-eval `(swank:set-default-directory
                                          ,(slime-to-lisp-filename dir)))))
    (with-current-buffer (slime-output-buffer)
      (setq default-directory dir))))

(defun slime-sync-package-and-default-directory ()
  "Set Lisp's package and directory to the values in current buffer."
  (interactive)
  (let ((package (slime-repl-shortcut-eval `(swank:set-package 
                                             ,(slime-find-buffer-package))))
	(directory (slime-from-lisp-filename
                    (slime-repl-shortcut-eval `(swank:set-default-directory 
                                                ,(slime-to-lisp-filename
                                                  default-directory))))))
    (let ((dir default-directory))
      ;; Sync REPL dir
      (with-current-buffer (slime-output-buffer)
        (setq default-directory dir))
      ;; Sync *inferior-lisp* dir
      (let* ((proc (slime-process))
             (buffer (and proc (process-buffer proc))))
        (when buffer 
          (with-current-buffer buffer
            (setq default-directory dir)))))
    (message "package: %s  default-directory: %s" (car package) directory)))
	

;;;; Debugger (SLDB)

(defvar sldb-hook nil
  "Hook run on entry to the debugger.")


;;;;; Local variables in the debugger buffer

(slime-make-variables-buffer-local
 (defvar sldb-condition nil
   "A list (DESCRIPTION TYPE) describing the condition being debugged.")

 (defvar sldb-saved-window-configuration nil
   "Window configuration before the debugger was initially entered.")

 (defvar sldb-restarts nil
   "List of (NAME DESCRIPTION) for each available restart.")

 (defvar sldb-level nil
   "Current debug level (recursion depth) displayed in buffer.")

 (defvar sldb-backtrace-start-marker nil
   "Marker placed at the beginning of the backtrace text.")

 (defvar sldb-continuations nil
   "List of ids for pending continuation."))

;;;;; SLDB macros

;; some macros that we need to define before the first use

(defmacro in-sldb-face (name string)
  "Return STRING propertised with face sldb-NAME-face."
  (let ((facename (intern (format "sldb-%s-face" (symbol-name name))))
	(var (gensym "string")))
    `(let ((,var ,string))
      (slime-add-face ',facename ,var)
      ,var)))

(put 'in-sldb-face 'lisp-indent-function 1)

(defun slime-add-face (face string)
  (add-text-properties 0 (length string) (list 'face face) string)
  string)


;;;;; sldb-mode

(defvar sldb-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    ;; We give < and > parenthesis syntax, so that #< ... > is treated
    ;; as a balanced expression.  This enables autodoc-mode to match
    ;; #<unreadable> actual arguments in the backtraces with formal
    ;; arguments of the function.  (For Lisp mode, this is not
    ;; desirable, since we do not wish to get a mismatched paren
    ;; highlighted everytime we type < or >.)
    (modify-syntax-entry ?< "(" table)
    (modify-syntax-entry ?> ")" table)
    table)
  "Syntax table for SLDB mode.")

(define-derived-mode sldb-mode fundamental-mode "sldb"
  "Superior lisp debugger mode. In addition to ordinary SLIME commands,
the following are available:\\<sldb-mode-map>

Commands to examine the selected frame:
   \\[sldb-toggle-details]   - toggle details (local bindings, CATCH tags)
   \\[sldb-show-source]   - view source for the frame
   \\[sldb-eval-in-frame]   - eval in frame
   \\[sldb-pprint-eval-in-frame]   - eval in frame, pretty-print result
   \\[sldb-disassemble]   - disassemble
   \\[sldb-inspect-in-frame]   - inspect

Commands to invoke restarts:
   \\[sldb-quit]   - quit
   \\[sldb-abort]   - abort
   \\[sldb-continue]   - continue
   \\[sldb-invoke-restart-0]-\\[sldb-invoke-restart-9] - restart shortcuts

Commands to navigate frames:
   \\[sldb-down]   - down
   \\[sldb-up]   - up
   \\[sldb-details-down] - down, with details
   \\[sldb-details-up] - up, with details

Miscellaneous commands:
   \\[sldb-restart-frame]   - restart frame
   \\[sldb-return-from-frame]   - return from frame
   \\[sldb-step]   - step
   \\[sldb-break-with-default-debugger]   - switch to default debugger
   \\[slime-interactive-eval]   - eval

Full list of commands:

\\{sldb-mode-map}"
  (erase-buffer)
  (set-syntax-table sldb-mode-syntax-table)
  (slime-set-truncate-lines)
  ;; Make original slime-connection "sticky" for SLDB commands in this buffer
  (setq slime-buffer-connection (slime-connection)))

(slime-define-keys sldb-mode-map
  ("h"    'describe-mode)
  ("v"    'sldb-show-source)
  ((kbd "RET") 'sldb-default-action)
  ("\C-m"      'sldb-default-action)
  ([return] 'sldb-default-action)
  ([mouse-2]  'sldb-default-action/mouse)
  ([follow-link] 'mouse-face)
  ("e"    'sldb-eval-in-frame)
  ("d"    'sldb-pprint-eval-in-frame)
  ("D"    'sldb-disassemble)
  ("i"    'sldb-inspect-in-frame)
  ("n"    'sldb-down)
  ("p"    'sldb-up)
  ("\M-n" 'sldb-details-down)
  ("\M-p" 'sldb-details-up)
  ("<"    'sldb-beginning-of-backtrace)
  (">"    'sldb-end-of-backtrace)
  ("t"    'sldb-toggle-details)
  ("r"    'sldb-restart-frame)
  ("I"    'sldb-invoke-restart-by-name)
  ("R"    'sldb-return-from-frame)
  ("c"    'sldb-continue)
  ("s"    'sldb-step)
  ("x"    'sldb-next)
  ("o"    'sldb-out)
  ("b"    'sldb-break-on-return)
  ("a"    'sldb-abort)
  ("q"    'sldb-quit)
  ("B"    'sldb-break-with-default-debugger)
  ("P"    'sldb-print-condition)
  ("C"    'sldb-inspect-condition)
  (":"    'slime-interactive-eval)
  ("\C-c\C-c" 'sldb-recompile-frame-source)
  ("\C-c\C-d" slime-doc-map))

;; Inherit bindings from slime-mode
(dolist (spec slime-keys)
  (destructuring-bind (key command &key sldb prefixed &allow-other-keys) spec
    (when sldb
      (let ((key (if prefixed (concat slime-prefix-key key) key)))
        (define-key sldb-mode-map key command)))))

;; Keys 0-9 are shortcuts to invoke particular restarts.
(dotimes (number 10)
  (let ((fname (intern (format "sldb-invoke-restart-%S" number)))
        (docstring (format "Invoke restart numbered %S." number)))
    (eval `(defun ,fname ()
             ,docstring
             (interactive)
             (sldb-invoke-restart ,number)))
    (define-key sldb-mode-map (number-to-string number) fname)))


;;;;; SLDB buffer creation & update

(defun sldb-buffers ()
  "Return a list of all sldb buffers."
  (slime-filter-buffers (lambda () (eq major-mode 'sldb-mode))))

(defun sldb-find-buffer (thread &optional connection)
  (let ((connection (or connection (slime-connection))))
    (find-if (lambda (buffer)
               (with-current-buffer buffer
                 (and (eq slime-buffer-connection connection)
                      (eq slime-current-thread thread))))
             (sldb-buffers))))

(defun sldb-get-default-buffer ()
  "Get a sldb buffer.
The buffer is chosen more or less randomly."
  (car (sldb-buffers)))

(defun sldb-get-buffer (thread &optional connection)
  "Find or create a sldb-buffer for THREAD."
  (let ((connection (or connection (slime-connection))))
    (or (sldb-find-buffer thread connection)
        (let ((name (format "*sldb %s/%s*" (slime-connection-name) thread)))
          (with-current-buffer (generate-new-buffer name)
            (setq slime-buffer-connection connection
                  slime-current-thread thread)
            (current-buffer))))))

(defun sldb-debugged-continuations (connection)
  "Return the debugged continuations for CONNECTION."
  (lexical-let ((accu '()))
    (dolist (b (sldb-buffers))
      (with-current-buffer b
        (when (eq slime-buffer-connection connection)
          (setq accu (append sldb-continuations accu)))))
    accu))

(defun sldb-setup (thread level condition restarts frames conts)
  "Setup a new SLDB buffer.
CONDITION is a string describing the condition to debug.
RESTARTS is a list of strings (NAME DESCRIPTION) for each available restart.
FRAMES is a list (NUMBER DESCRIPTION &optional PLIST) describing the initial
portion of the backtrace. Frames are numbered from 0.
CONTS is a list of pending Emacs continuations."
  (with-current-buffer (sldb-get-buffer thread)
    (unless (equal sldb-level level)
      (setq buffer-read-only nil)
      (sldb-mode)
      (unless sldb-saved-window-configuration
        (setq sldb-saved-window-configuration (current-window-configuration)))
      (setq slime-current-thread thread)
      (setq sldb-level level)
      (setq mode-name (format "sldb[%d]" sldb-level))
      (setq sldb-condition condition)
      (setq sldb-restarts restarts)
      (setq sldb-continuations conts)
      (sldb-insert-condition condition)
      (insert "\n\n" (in-sldb-face section "Restarts:") "\n")
      (sldb-insert-restarts restarts)
      (insert "\n" (in-sldb-face section "Backtrace:") "\n")
      (setq sldb-backtrace-start-marker (point-marker))
      (save-excursion
        (if frames 
            (sldb-insert-frames (sldb-prune-initial-frames frames) t)
          (insert "[No backtrace]")))
      (run-hooks 'sldb-hook))
    (pop-to-buffer (current-buffer))
    (sldb-recenter-region (point-min) (point))
    (setq buffer-read-only t)
    (when (and slime-stack-eval-tags
               ;; (y-or-n-p "Enter recursive edit? ")
               )
      (message "Entering recursive edit..")
      (recursive-edit))))

(defun sldb-activate (thread level select)
  "Display the debugger buffer for THREAD.
If LEVEL isn't the same as in the buffer, reinitialize the buffer."
  (or (let ((buffer (sldb-find-buffer thread)))
        (when buffer
          (with-current-buffer buffer
            (when (equal sldb-level level)
              (when select (pop-to-buffer (current-buffer)))
              t))))
      (sldb-reinitialize thread level)))

(defun sldb-reinitialize (thread level)
  (slime-rex (thread level)
      ('(swank:debugger-info-for-emacs 0 10)
       nil thread)
    ((:ok result)
     (apply #'sldb-setup thread level result))))

(defun sldb-exit (thread level &optional stepping)
  "Exit from the debug level LEVEL."
  (when-let (sldb (sldb-find-buffer thread))
    (with-current-buffer sldb
      (unless stepping
        (set-window-configuration sldb-saved-window-configuration))
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq sldb-level nil))
    (cond ((and (= level 1) (not stepping))
           (kill-buffer sldb))
          (t (sldb-maybe-kill-buffer thread (slime-connection))))))

;; If we return to a lower debug level we wait a little before closing
;; the debugger window.  We also send a ping, just in case Lisp was
;; interrupted in swank:wait-for-input.
(defun sldb-maybe-kill-buffer (thread connection)
  (run-with-idle-timer
   0.3 nil 
   (lambda (thead connection)
     (when-let (sldb (sldb-find-buffer thread connection))
       (with-current-buffer sldb
         (when (not sldb-level)
           (kill-buffer sldb)))))
   thread connection))


;;;;;; SLDB buffer insertion

(defun sldb-insert-condition (condition)
  "Insert the text for CONDITION.
CONDITION should be a list (MESSAGE TYPE EXTRAS).
EXTRAS is currently used for the stepper."
  (destructuring-bind (message type extras) condition
    (when (> (length message) 70)
      (add-text-properties 0 (length message) (list 'help-echo message)
                           message))
    (slime-insert-propertized '(sldb-default-action sldb-inspect-condition)
                              (in-sldb-face topline message)
                              "\n"
                              (in-sldb-face condition type))
    (sldb-dispatch-extras extras)))

(defvar sldb-extras-hooks)

(defun sldb-dispatch-extras (extras)
  ;; this is (mis-)used for the stepper
  (dolist (extra extras)
    (destructure-case extra
      ((:show-frame-source n)
       (sldb-show-frame-source n))
      (t
       (or (run-hook-with-args-until-success 'sldb-extras-hooks extra)
           ;;(error "Unhandled extra element:" extra)
           )))))

(defun sldb-insert-restarts (restarts)
  "Insert RESTARTS and add the needed text props
RESTARTS should be alist ((NAME DESCRIPTION) ...)."
  (loop for (name string) in restarts
        for number from 0 do
        (insert " ")
        (slime-insert-propertized
         `(,@nil restart-number ,number
                 sldb-default-action sldb-invoke-restart
                 mouse-face highlight)
         (in-sldb-face restart-number (number-to-string number))
         ": ["  (in-sldb-face restart-type name) "] "
         (in-sldb-face restart string))
        (insert "\n")))

(defun sldb-frame.string (frame)
  (destructuring-bind (_ str &optional _) frame str))

(defun sldb-frame.number (frame)
  (destructuring-bind (n _ &optional _) frame n))

(defun sldb-frame.plist (frame)
  (destructuring-bind (_ _ &optional plist) frame plist))

(defun sldb-prune-initial-frames (frames)
  "Return the prefix of FRAMES to initially present to the user.
Regexp heuristics are used to avoid showing SWANK-internal frames."
  (let* ((case-fold-search t)
         (rx "^\\([() ]\\|lambda\\)*swank\\>"))
    (or (loop for frame in frames
              until (string-match rx (sldb-frame.string frame))
              collect frame)
        frames)))

(defun sldb-insert-frames (frames more)
  "Insert FRAMES into buffer.
If MORE is non-nil, more frames are on the Lisp stack."
  (mapc #'sldb-insert-frame frames)
  (when more
    (slime-insert-propertized
     `(,@nil sldb-default-action sldb-fetch-more-frames
             sldb-previous-frame-number ,(sldb-frame.number (first (last frames)))
             point-entered sldb-fetch-more-frames
             start-open t
             face sldb-section-face
             mouse-face highlight)
     " --more--")
    (insert "\n")))

(defun sldb-compute-frame-face (frame)
  (let ((restartable (getf (sldb-frame.plist frame) :restartable)))
    (cond ((eq restartable 't)
           'sldb-restartable-frame-line-face)
          ((eq restartable :unknown)
           'sldb-frame-line-face)
          ((eq restartable 'nil)
           'sldb-non-restartable-frame-line-face)
          (t (error "fall through")))))

(defun sldb-insert-frame (frame &optional face)
  "Insert FRAME with FACE at point.
If FACE is nil, `sldb-compute-frame-face' is used to determine the face."
  (setq face (or face (sldb-compute-frame-face frame)))
  (let ((number (sldb-frame.number frame))
        (string (sldb-frame.string frame))
        (props `(frame ,frame sldb-default-action sldb-toggle-details)))
    (slime-propertize-region props
      (slime-propertize-region '(mouse-face highlight)
        (insert " " (in-sldb-face frame-label (format "%2d:" number)) " ")
        (slime-insert-indented
         (slime-add-face face string)))
      (insert "\n"))))

(defun sldb-fetch-more-frames (&rest ignore)
  "Fetch more backtrace frames.
Called on the `point-entered' text-property hook."
  (let ((inhibit-point-motion-hooks t)
        (inhibit-read-only t)
        (prev (get-text-property (point) 'sldb-previous-frame-number)))
    ;; we may be called twice, PREV is nil the second time
    (when prev
      (let* ((count 40)
             (from (1+ prev))
             (to (+ from count))
             (frames (slime-eval `(swank:backtrace ,from ,to)))
             (more (slime-length= frames count))
             (pos (point)))
        (delete-region (line-beginning-position) (point-max))
        (sldb-insert-frames frames more)
        (goto-char pos)))))


;;;;;; SLDB examining text props

(defun sldb-restart-at-point ()
  (or (get-text-property (point) 'restart-number)
      (error "No restart at point")))

(defun sldb-frame-number-at-point ()
  (let ((frame (get-text-property (point) 'frame)))
    (cond (frame (car frame))
	  (t (error "No frame at point")))))

(defun sldb-var-number-at-point ()
  (let ((var (get-text-property (point) 'var)))
    (cond (var var)
	  (t (error "No variable at point")))))

(defun sldb-previous-frame-number ()
  (save-excursion
    (sldb-backward-frame)
    (sldb-frame-number-at-point)))

(defun sldb-frame-details-visible-p ()
  (and (get-text-property (point) 'frame)
       (get-text-property (point) 'details-visible-p)))

(defun sldb-frame-region ()
  (slime-property-bounds 'frame))

(defun sldb-forward-frame ()
  (goto-char (next-single-char-property-change (point) 'frame)))

(defun sldb-backward-frame ()
  (goto-char (previous-single-char-property-change
              (car (sldb-frame-region))
              'frame
              nil sldb-backtrace-start-marker)))

(defun sldb-goto-last-frame ()
  (goto-char (point-max))
  (while (not (get-text-property (point) 'frame))
    (goto-char (previous-single-property-change (point) 'frame))))

(defun sldb-beginning-of-backtrace ()
  "Goto the first frame."
  (interactive)
  (goto-char sldb-backtrace-start-marker))


;;;;;; SLDB recenter & redisplay

;; FIXME: these functions need factorization

(defvar sldb-show-location-recenter-arg nil
  "Argument to pass to `recenter' when displaying a source location.")

(defun slime-show-buffer-position (position)
  "Ensure sure that the POSITION in the current buffer is visible."
  (save-selected-window
    (let ((w (select-window (or (get-buffer-window (current-buffer) t)
                                (display-buffer (current-buffer) t)))))
      (goto-char position)
      (push-mark)
      (unless (pos-visible-in-window-p)
        (slime-recenter-window w sldb-show-location-recenter-arg)))))

(defun slime-recenter-window (window line)
  "Set window-start in WINDOW LINE lines before point."
  (let* ((line (if (not line)
                   (/ (window-height window) 2)
                 line))
         (start (save-excursion
                  (loop repeat line do (forward-line -1))
                  (point))))
    (set-window-start window start)))

(defun sldb-recenter-region (start end &optional center)
  "Make the region from START to END visible.
Avoid point motions, if possible.
Minimize scrolling, if CENTER is nil.
If CENTER is true, scroll enough to center the region in the window."
  (let ((pos (point))  (lines (count-screen-lines start end t)))
    (assert (and (<= start pos) (<= pos end)))
    ;;(sit-for 0)
    (cond ((and (pos-visible-in-window-p start)
                (pos-visible-in-window-p end)))
          ((< lines (window-height))
           (cond (center (recenter (+ (/ (- (window-height) 1 lines)
                                         2)
                                      (slime-count-lines start pos))))
                 (t (recenter (+ (- (window-height) 1 lines)
                                 (slime-count-lines start pos))))))
          (t
           (goto-char start)
           (recenter 0)
           (cond ((pos-visible-in-window-p pos)
                  (goto-char pos))
                 (t
                  (goto-char start)
                  (unless noninteractive ; for running the test suite
                    (next-line (- (window-height) 2)))))))))

;; not sure yet, whether this is a good idea.
(defmacro slime-save-coordinates (origin &rest body)
  "Restore line and column relative to ORIGIN, after executing BODY.

This is useful if BODY deletes and inserts some text but we want to
preserve the current row and column as closely as possible."
  (let ((base (make-symbol "base"))
        (goal (make-symbol "goal"))
        (mark (make-symbol "mark")))
    `(let* ((,base ,origin)
            (,goal (slime-coordinates ,base))
            (,mark (point-marker)))
       (set-marker-insertion-type ,mark t)
       (prog1 (save-excursion ,@body)
         (slime-restore-coordinate ,base ,goal ,mark)))))

(put 'slime-save-coordinates 'lisp-indent-function 1)

(defun slime-coordinates (origin)
  ;; Return a pair (X . Y) for the column and line distance to ORIGIN.
  (let ((y (slime-count-lines origin (point)))
        (x (save-excursion
             (- (current-column)
                (progn (goto-char origin) (current-column))))))
    (cons x y)))

(defun slime-restore-coordinate (base goal limit)
  ;; Move point to GOAL. Coordinates are relative to BASE.
  ;; Don't move beyond LIMIT.
  (save-restriction
    (narrow-to-region base limit)
    (goto-char (point-min))
    (let ((col (current-column)))
      (forward-line (cdr goal))
      (when (and (eobp) (bolp) (not (bobp)))
        (backward-char))
      (move-to-column (+ col (car goal))))))

(defun slime-count-lines (start end)
  "Return the number of lines between START and END.
This is 0 if START and END at the same line."
  (- (count-lines start end)
     (if (save-excursion (goto-char end) (bolp)) 0 1)))


;;;;; SLDB commands

(defun sldb-default-action ()
  "Invoke the action at point."
  (interactive)
  (let ((fn (get-text-property (point) 'sldb-default-action)))
    (if fn (funcall fn))))

(defun sldb-default-action/mouse (event)
  "Invoke the action pointed at by the mouse."
  (interactive "e")
  (destructuring-bind (mouse-1 (w pos &rest _)) event
    (save-excursion
      (goto-char pos)
      (let ((fn (get-text-property (point) 'sldb-default-action)))
	(if fn (funcall fn))))))

(defun sldb-end-of-backtrace ()
  "Fetch the entire backtrace and go to the last frame."
  (interactive)
  (sldb-fetch-all-frames)
  (sldb-goto-last-frame))

(defun sldb-fetch-all-frames ()
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (sldb-goto-last-frame)
    (let ((last (sldb-frame-number-at-point)))
      (goto-char (next-single-char-property-change (point) 'frame))
      (delete-region (point) (point-max))
      (save-excursion
        (sldb-insert-frames (slime-eval `(swank:backtrace ,(1+ last) nil))
                            nil)))))


;;;;;; SLDB show source

(defun sldb-show-source ()
  "Highlight the frame at point's expression in a source code buffer."
  (interactive)
  (sldb-show-frame-source (sldb-frame-number-at-point)))

(defun sldb-show-frame-source (frame-number)
  (slime-eval-async
   `(swank:frame-source-location-for-emacs ,frame-number)
   (lambda (source-location)
     (destructure-case source-location
       ((:error message)
        (message "%s" message)
        (ding))
       (t
        (slime-show-source-location source-location))))))

(defun slime-show-source-location (source-location &optional no-highlight-p)
  (save-selected-window   ; show the location, but don't hijack focus.
    (slime-goto-source-location source-location)
    (unless no-highlight-p (sldb-highlight-sexp))
    (slime-show-buffer-position (point))))

(defun sldb-highlight-sexp (&optional start end)
  "Highlight the first sexp after point."
  (let ((start (or start (point)))
	(end (or end (save-excursion (ignore-errors (forward-sexp)) (point)))))
    (slime-flash-region start end)))



;;;;;; SLDB toggle details

(defun sldb-toggle-details (&optional on)
  "Toggle display of details for the current frame.
The details include local variable bindings and CATCH-tags."
  (interactive)
  (assert (sldb-frame-number-at-point))
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (if (or on (not (sldb-frame-details-visible-p)))
	(sldb-show-frame-details)
      (sldb-hide-frame-details))))

(defun sldb-show-frame-details ()
  ;; fetch and display info about local variables and catch tags
  (destructuring-bind (start end frame locals catches) (sldb-frame-details)
    (slime-save-coordinates start
      (delete-region start end)
      (slime-propertize-region `(frame ,frame details-visible-p t)
        (sldb-insert-frame frame 'sldb-detailed-frame-line-face)
        (let ((indent1 "      ")
              (indent2 "        "))
          (insert indent1 (in-sldb-face section
                            (if locals "Locals:" "[No Locals]")) "\n")
          (sldb-insert-locals locals indent2 frame)
          (when catches
            (insert indent1 (in-sldb-face section "Catch-tags:") "\n")
            (dolist (tag catches)
              (slime-propertize-region `(catch-tag ,tag)
                (insert indent2 (in-sldb-face catch-tag (format "%s" tag))
                        "\n"))))
          (setq end (point)))))
    (sldb-recenter-region start end)))

(defun sldb-frame-details ()
  ;; Return a list (START END FRAME LOCALS CATCHES) for frame at point.
  (let* ((frame (get-text-property (point) 'frame))
         (num (car frame))
         (catches (sldb-catch-tags num))
         (locals  (sldb-frame-locals num)))
    (destructuring-bind (start end) (sldb-frame-region)
      (list start end frame locals catches))))

(defvar sldb-insert-frame-variable-value-function 'sldb-insert-frame-variable-value)

(defun sldb-insert-locals (vars prefix frame)
  "Insert VARS and add PREFIX at the beginning of each inserted line.
VAR should be a plist with the keys :name, :id, and :value."
  (loop for i from 0
        for var in vars do
        (destructuring-bind (&key name id value) var
          (slime-propertize-region (list 'sldb-default-action 'sldb-inspect-var
                                         'var i)
            (insert prefix
                    (in-sldb-face local-name
                      (concat name (if (zerop id) "" (format "#%d" id))))
                    " = ")
            (funcall sldb-insert-frame-variable-value-function value frame i)
            (insert "\n")))))

(defun sldb-insert-frame-variable-value (value frame index)
  (insert (in-sldb-face local-value value)))  

(defun sldb-hide-frame-details ()
  ;; delete locals and catch tags, but keep the function name and args.
  (destructuring-bind (start end) (sldb-frame-region)
    (let ((frame (get-text-property (point) 'frame)))
      (slime-save-coordinates start
        (delete-region start end)
        (slime-propertize-region '(details-visible-p nil)
          (sldb-insert-frame frame))))))

(defun sldb-disassemble ()
  "Disassemble the code for the current frame."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:sldb-disassemble ,frame)
                      (lambda (result)
			(slime-show-description result nil)))))

(defun sldb-print-condition ()
  "Print the condition SLDB is handling in the REPL.
This way you can still see what the error was after exiting SLDB."
  (interactive)
  (unless sldb-condition
    (error "No condition known (wrong buffer?)"))
  (slime-write-string (format "%s\n%s\n"
                               (first sldb-condition)
                               (second sldb-condition))))

(defun sldb-frame-locals (frame)
  (slime-eval `(swank:frame-locals-for-emacs ,frame)))

(defun sldb-catch-tags (frame)
  (slime-eval `(swank:frame-catch-tags-for-emacs ,frame)))


;;;;;; SLDB eval and inspect

(defun sldb-eval-in-frame (string)
  "Prompt for an expression and evaluate it in the selected frame."
  (interactive (list (slime-read-from-minibuffer "Eval in frame: ")))
  (let* ((number (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:eval-string-in-frame ,string ,number)
                      (if current-prefix-arg
                          'slime-write-string
                        'slime-display-eval-result))))

(defun sldb-pprint-eval-in-frame (string)
  "Prompt for an expression, evaluate in selected frame, pretty-print result."
  (interactive (list (slime-read-from-minibuffer "Eval in frame: ")))
  (let* ((number (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:pprint-eval-string-in-frame ,string ,number)
		      (lambda (result)
			(slime-show-description result nil)))))



(defun sldb-inspect-in-frame (string)
  "Prompt for an expression and inspect it in the selected frame."
  (interactive (list (slime-read-from-minibuffer 
                      "Inspect in frame (evaluated): " 
                      (slime-sexp-at-point))))
  (let ((number (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:inspect-in-frame ,string ,number)
                      'slime-open-inspector)))

(defun sldb-inspect-var ()
  (let ((frame (sldb-frame-number-at-point))
        (var (sldb-var-number-at-point)))
    (slime-eval-async `(swank:inspect-frame-var ,frame ,var) 
                      'slime-open-inspector)))

(defun sldb-inspect-condition ()
  "Inspect the current debugger condition."
  (interactive)
  (slime-eval-async '(swank:inspect-current-condition)
                    'slime-open-inspector))


;;;;;; SLDB movement

(defun sldb-down ()
  "Select next frame."
  (interactive)
  (sldb-forward-frame))

(defun sldb-up ()
  "Select previous frame."
  (interactive)
  (sldb-backward-frame)
  (when (= (point) sldb-backtrace-start-marker)
    (recenter (1+ (count-lines (point-min) (point))))))

(defun sldb-sugar-move (move-fn)
  (let ((inhibit-read-only t))
    (when (sldb-frame-details-visible-p) (sldb-hide-frame-details))
    (funcall move-fn)
    (sldb-show-source)
    (sldb-toggle-details t)))

(defun sldb-details-up ()
  "Select previous frame and show details."
  (interactive)
  (sldb-sugar-move 'sldb-up))

(defun sldb-details-down ()
  "Select next frame and show details."
  (interactive)
  (sldb-sugar-move 'sldb-down))


;;;;;; SLDB restarts

(defun sldb-quit ()
  "Quit to toplevel."
  (interactive)
  (assert sldb-restarts () "sldb-quit called outside of sldb buffer")
  (slime-rex () ('(swank:throw-to-toplevel))
    ((:ok _) (error "sldb-quit returned"))
    ((:abort))))

(defun sldb-continue ()
  "Invoke the \"continue\" restart."
  (interactive)
  (assert sldb-restarts () "sldb-continue called outside of sldb buffer")
  (slime-rex ()
      ('(swank:sldb-continue))
    ((:ok _)
     (message "No restart named continue")
     (ding))
    ((:abort))))

(defun sldb-abort ()
  "Invoke the \"abort\" restart."
  (interactive)
  (slime-eval-async '(swank:sldb-abort)
                    (lambda (v) (message "Restart returned: %S" v))))

(defun sldb-invoke-restart (&optional number)
  "Invoke a restart.
Optional NUMBER specifies the restart to invoke, otherwise
use the restart at point."
  (interactive)
  (let ((restart (or number (sldb-restart-at-point))))
    (slime-rex ()
        ((list 'swank:invoke-nth-restart-for-emacs sldb-level restart))
      ((:ok value) (message "Restart returned: %s" value))
      ((:abort)))))

(defun sldb-invoke-restart-by-name (restart-name)
  (interactive (list (completing-read "Restart: "
                                      sldb-restarts nil t
                                      ""
                                      'sldb-invoke-restart-by-name)))
  (sldb-invoke-restart (position restart-name sldb-restarts 
                                 :test 'string= :key 'first)))

(defun sldb-break-with-default-debugger ()
  "Enter default debugger."
  (interactive)
  (slime-rex ()
      ('(swank:sldb-break-with-default-debugger) nil slime-current-thread)
    ((:abort))))

(defun sldb-step ()
  "Select the \"continue\" restart and set a new break point."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:sldb-step ,frame))))

(defun sldb-next ()
  "Select the \"continue\" restart and set a new break point."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:sldb-next ,frame))))

(defun sldb-out ()
  "Select the \"continue\" restart and set a new break point."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:sldb-out ,frame))))

(defun sldb-break-on-return ()
  "Set a breakpoint at the current frame.
The debugger is entered when the frame exits."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:sldb-break-on-return ,frame)
                      (lambda (msg) (message "%s" msg)))))

(defun sldb-break (name)
  "Set a breakpoint at the start of the function NAME."
  (interactive (list (slime-read-symbol-name "Function: " t)))
  (slime-eval-async `(swank:sldb-break ,name)
                    (lambda (msg) (message "%s" msg))))

(defun sldb-return-from-frame (string)
  "Reads an expression in the minibuffer and causes the function to
return that value, evaluated in the context of the frame."
  (interactive (list (slime-read-from-minibuffer "Return from frame: ")))
  (let* ((number (sldb-frame-number-at-point)))
    (slime-rex ()
        ((list 'swank:sldb-return-from-frame number string))
      ((:ok value) (message "%s" value))
      ((:abort)))))

(defun sldb-restart-frame ()
  "Causes the frame to restart execution with the same arguments as it
was called originally."
  (interactive)
  (let* ((number (sldb-frame-number-at-point)))
    (slime-rex ()
        ((list 'swank:restart-frame number))
      ((:ok value) (message "%s" value))
      ((:abort)))))


;;;;;; SLDB recompilation commands

(defun sldb-recompile-frame-source (&optional raw-prefix-arg)
  (interactive "P")
  (slime-eval-async
   `(swank:frame-source-location-for-emacs ,(sldb-frame-number-at-point))
   (lexical-let ((debug-level (slime-normalize-optimization-level
                               (and raw-prefix-arg 
                                    (prefix-numeric-value raw-prefix-arg)))))
     (lambda (source-location)
       (destructure-case source-location
         ((:error message)
          (message "%s" message)
          (ding))
         (t
          (slime-recompile-location source-location debug-level)))))))






;;;; Thread control panel

(defvar slime-threads-buffer-name "*SLIME Threads*")

(defun slime-list-threads ()
  "Display a list of threads."
  (interactive)
  (let ((name slime-threads-buffer-name))
    (slime-with-popup-buffer (name nil t)
      (slime-thread-control-mode)
      (setq slime-popup-buffer-quit-function 'slime-quit-threads-buffer)
      (slime-update-threads-buffer))))

(defun slime-quit-threads-buffer (&optional _)
  (slime-eval-async `(swank:quit-thread-browser))
  (slime-popup-buffer-quit t))

(defun slime-update-threads-buffer ()
  (interactive)
  (let ((threads (slime-eval '(swank:list-threads))))
    (with-current-buffer slime-threads-buffer-name
      (let ((inhibit-read-only t))
        (erase-buffer)
        (loop for idx from 0 
              for (id name status desc) in threads
              do (slime-thread-insert idx name status desc id))
        (goto-char (point-min))))))

(defun slime-thread-insert (idx name status summary id)
  (slime-propertize-region `(thread-id ,idx)
    (insert (format "%3s: " id))
    (slime-insert-propertized '(face bold) name)
    (insert-char ?\  (- 30 (current-column)))
    (let ((summary-start (point)))
      (insert " " status)
      (insert " " summary)
      (unless (bolp) (insert "\n"))
      (indent-rigidly summary-start (point) 2))))


;;;;; Major mode

(define-derived-mode slime-thread-control-mode fundamental-mode
  "Threads"
  "SLIME Thread Control Panel Mode.

\\{slime-thread-control-mode-map}
\\{slime-popup-buffer-mode-map}"
  (when slime-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

(slime-define-keys slime-thread-control-mode-map
  ("a"         'slime-thread-attach)
  ("d"         'slime-thread-debug)
  ("g"         'slime-update-threads-buffer)
  ("k"         'slime-thread-kill))


(defun slime-thread-kill ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-id)))
    (slime-eval `(swank:kill-nth-thread ,id)))
  (call-interactively 'slime-list-threads))

(defun slime-thread-attach ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-id))
        (file (slime-swank-port-file)))
    (slime-eval-async `(swank:start-swank-server-in-thread ,id ,file)))
  (slime-read-port-and-connect nil nil))

(defun slime-thread-debug ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-id)))
    (slime-eval-async `(swank:debug-nth-thread ,id))))


;;;;; Connection listing

(define-derived-mode slime-connection-list-mode fundamental-mode
  "Slime-Connections"
  "SLIME Connection List Mode.

\\{slime-connection-list-mode-map}
\\{slime-popup-buffer-mode-map}"
  (when slime-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

(slime-define-keys slime-connection-list-mode-map
  ((kbd "RET") 'slime-goto-connection)
  ([return] 'slime-goto-connection)
  ("d"         'slime-connection-list-make-default)
  ("g"         'slime-update-connection-list)
  ((kbd "C-k") 'slime-quit-connection-at-point)
  ("R"         'slime-restart-connection-at-point))

(defun slime-connection-at-point ()
  (or (get-text-property (point) 'slime-connection)
      (error "No connection at point")))

(defun slime-goto-connection ()
  "Switch to the REPL buffer for the connection at point."
  (interactive)
  (let ((slime-dispatching-connection (slime-connection-at-point)))
    (switch-to-buffer (slime-output-buffer))))

(defun slime-quit-connection-at-point (connection)
  (interactive (list (slime-connection-at-point)))
  (let ((slime-dispatching-connection connection)
        (end (time-add (current-time) (seconds-to-time 3))))
    (slime-quit-lisp t)
    (while (memq connection slime-net-processes)
      (when (time-less-p end (current-time))
        (message "Quit timeout expired.  Disconnecting.")
        (delete-process connection))
      (sit-for 0 100)))
  (slime-update-connection-list))

(defun slime-restart-connection-at-point (connection)
  (interactive (list (slime-connection-at-point)))
  (let ((slime-dispatching-connection connection))
    (slime-restart-inferior-lisp)))
  
(defun slime-connection-list-make-default ()
  "Make the connection at point the default connection."
  (interactive)
  (slime-select-connection (slime-connection-at-point))
  (slime-update-connection-list))

(defvar slime-connections-buffer-name "*SLIME Connections*")

(defun slime-list-connections ()
  "Display a list of all connections."
  (interactive)
  (slime-with-popup-buffer (slime-connections-buffer-name)
    (slime-connection-list-mode)
    (slime-draw-connection-list)))

(defun slime-update-connection-list ()
 "Display a list of all connections."
 (interactive)
 (let ((pos (point))
       (inhibit-read-only t))
   (erase-buffer)
   (slime-draw-connection-list)
   (goto-char pos)))

(defun slime-draw-connection-list ()
  (let ((default-pos nil)
        (default slime-default-connection)
        (fstring "%s%2s  %-10s  %-17s  %-7s %-s\n"))
    (insert (format fstring " " "Nr" "Name" "Port" "Pid" "Type")
            (format fstring " " "--" "----" "----" "---" "----"))
    (dolist (p (reverse slime-net-processes))
      (when (eq default p) (setf default-pos (point)))
      (slime-insert-propertized 
       (list 'slime-connection p)
       (format fstring
               (if (eq default p) "*" " ")
               (slime-connection-number p)
               (slime-connection-name p)
               (or (process-id p) (process-contact p))
               (slime-pid p)
               (slime-lisp-implementation-type p))))
    (when default 
      (goto-char default-pos))))


;;;; Inspector

(defgroup slime-inspector nil
  "Inspector faces."
  :prefix "slime-inspector-"
  :group 'slime)

(defface slime-inspector-topline-face
  '((t ()))
  "Face for top line describing object."
  :group 'slime-inspector)

(defface slime-inspector-label-face
  '((t (:inherit font-lock-constant-face)))
  "Face for labels in the inspector."
  :group 'slime-inspector)

(defface slime-inspector-value-face
  (if (slime-face-inheritance-possible-p)
      '((t (:inherit font-lock-builtin-face)))
    '((((background light)) (:foreground "MediumBlue" :bold t))
      (((background dark)) (:foreground "LightGray" :bold t))))
  "Face for things which can themselves be inspected."
  :group 'slime-inspector)

(defface slime-inspector-action-face
  (if (slime-face-inheritance-possible-p)
      '((t (:inherit font-lock-warning-face)))
    '((t (:foreground "OrangeRed"))))
  "Face for labels of inspector actions."
  :group 'slime-inspector)

(defface slime-inspector-type-face
    '((t (:inherit font-lock-type-face)))
  "Face for type description in inspector."
  :group 'slime-inspector)

(defvar slime-inspector-mark-stack '())
(defvar slime-saved-window-config)

(defun slime-inspect (string)
  "Eval an expression and inspect the result."
  (interactive 
   (list (slime-read-from-minibuffer "Inspect value (evaluated): "
				     (slime-sexp-at-point))))
  (slime-eval-async `(swank:init-inspector ,string) 'slime-open-inspector))

(define-derived-mode slime-inspector-mode fundamental-mode "Slime-Inspector"
  (set-syntax-table lisp-mode-syntax-table)
  (slime-set-truncate-lines)
  (setq buffer-read-only t))

(defun slime-inspector-buffer ()
  (or (get-buffer "*Slime Inspector*")
      (with-current-buffer (get-buffer-create "*Slime Inspector*")
	(setq slime-inspector-mark-stack '())
        (buffer-disable-undo)
        (slime-mode t)
	(slime-inspector-mode)
        (make-local-variable 'slime-saved-window-config)
        (setq slime-saved-window-config (current-window-configuration))
	(current-buffer))))

(defmacro slime-inspector-fontify (face string)
  `(slime-add-face ',(intern (format "slime-inspector-%s-face" face)) ,string))

(defvar slime-inspector-insert-ispec-function 'slime-inspector-insert-ispec)

(defun slime-open-inspector (inspected-parts &optional point)
  "Display INSPECTED-PARTS in a new inspector window.
Optionally set point to POINT."
  (with-current-buffer (slime-inspector-buffer)
    (setq slime-buffer-connection (slime-current-connection))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (destructuring-bind (&key id title content) inspected-parts
        (macrolet ((fontify (face string) 
                            `(slime-inspector-fontify ,face ,string)))
          (slime-propertize-region
              (list 'slime-part-number id 
                 'mouse-face 'highlight
                 'face 'slime-inspector-value-face)
            (insert title))
          (while (eq (char-before) ?\n)
            (backward-delete-char 1))
          (insert "\n" (fontify label "--------------------") "\n")
          (save-excursion
            (slime-inspector-insert-content content))
          (pop-to-buffer (current-buffer))
          (when point
            (check-type point cons)
            (ignore-errors 
              (goto-line (car point))
              (move-to-column (cdr point)))))))))

(defvar slime-inspector-limit 500)

(defun slime-inspector-insert-content (content)
  (slime-inspector-fetch-chunk
   content nil 
   (lambda (chunk)
     (let ((inhibit-read-only t))
       (slime-inspector-insert-chunk chunk t t)))))

(defun slime-inspector-insert-chunk (chunk prev next)
  "Insert CHUNK at point.
If PREV resp. NEXT are true insert more-buttons as needed."
  (destructuring-bind (ispecs len start end) chunk
    (when (and prev (> start 0))
      (slime-inspector-insert-more-button start t))
    (mapc #'slime-inspector-insert-ispec ispecs)
    (when (and next (< end len))
      (slime-inspector-insert-more-button end nil))))

(defun slime-inspector-insert-ispec (ispec)
  (if (stringp ispec)
      (insert ispec)
    (destructure-case ispec
      ((:value string id)
       (slime-propertize-region 
           (list 'slime-part-number id 
                 'mouse-face 'highlight
                 'face 'slime-inspector-value-face)
         (insert string)))
      ((:action string id)
       (slime-insert-propertized (list 'slime-action-number id
                                       'mouse-face 'highlight
                                       'face 'slime-inspector-action-face)
                                 string)))))

(defun slime-inspector-position ()
  "Return a pair (Y-POSITION X-POSITION) representing the
position of point in the current buffer."
  ;; We make sure we return absolute coordinates even if the user has
  ;; narrowed the buffer.
  (save-restriction
    (widen)
    (cons (cond ((fboundp 'line-number)
                 (line-number))         ; XEmacs
                ((fboundp 'line-number-at-pos)
                 (line-number-at-pos))  ; Recent GNU Emacs
                (t (1+ (count-lines 1 (point-at-bol)))))
          (current-column))))

(defun slime-inspector-operate-on-point ()
  "Invoke the command for the text at point.
1. If point is on a value then recursivly call the inspector on
that value.  
2. If point is on an action then call that action.
3. If point is on a range-button fetch and insert the range."
  (interactive)
  (let ((part-number (get-text-property (point) 'slime-part-number))
        (range-button (get-text-property (point) 'slime-range-button))
        (action-number (get-text-property (point) 'slime-action-number))
        (opener (lexical-let ((point (slime-inspector-position)))
                  (lambda (parts)
                    (when parts
                      (slime-open-inspector parts point))))))
    (cond (part-number
           (slime-eval-async `(swank:inspect-nth-part ,part-number)
                             opener)
           (push (slime-inspector-position) slime-inspector-mark-stack))
          (range-button
           (slime-inspector-fetch-more range-button))
          (action-number 
           (slime-eval-async `(swank::inspector-call-nth-action ,action-number)
                             opener))
          (t (error "No object at point")))))

(defun slime-inspector-operate-on-click (event)
  "Inspect the value at the clicked-at position or invoke an action."
  (interactive "@e")
  (let ((point (posn-point (event-end event))))
    (cond ((and point
                (or (get-text-property point 'slime-part-number)
                    (get-text-property point 'slime-action-number)))
           (goto-char point)
           (slime-inspector-operate-on-point))
          (t
           (error "No clickable part here")))))

(defun slime-inspector-copy-down (number)
  "Evaluate the slot at point via the REPL (to set `*')."
  (interactive (list (or (get-text-property (point) 'slime-part-number)
                         (error "No part at point"))))
  (slime-repl-send-string (format "%s" `(swank:inspector-nth-part ,number)))
  (slime-repl))

(defun slime-inspector-pop ()
  (interactive)
  (slime-eval-async 
   `(swank:inspector-pop)
   (lambda (result)
     (cond (result
	    (slime-open-inspector result (pop slime-inspector-mark-stack)))
	   (t 
	    (message "No previous object")
	    (ding))))))

(defun slime-inspector-next ()
  (interactive)
  (let ((result (slime-eval `(swank:inspector-next))))
    (cond (result 
	   (push (slime-inspector-position) slime-inspector-mark-stack)
	   (slime-open-inspector result))
	  (t (message "No next object")
	     (ding)))))
  
(defun slime-inspector-quit ()
  (interactive)
  (slime-eval-async `(swank:quit-inspector))
  (set-window-configuration slime-saved-window-config)
  (kill-buffer (current-buffer)))

(defun slime-find-inspectable-object (direction limit)
  "Finds the next or previous inspectable object within the
current buffer, depending on whether DIRECTION is 'NEXT or
'PREV. LIMIT is the maximum or minimum position in the current
buffer.

Returns a list of two values: If an object could be found, the
starting position of the found object and T is returned;
otherwise LIMIT and NIL is returned.
"
  (let ((finder (ecase direction
                  (next 'next-single-property-change)
                  (prev 'previous-single-property-change))))
    (let ((prop nil) (curpos (point)))
      (while (and (not prop) (not (= curpos limit)))
        (let ((newpos (funcall finder curpos 'slime-part-number nil limit)))
          (setq prop (get-text-property newpos 'slime-part-number))
          (setq curpos newpos)))
      (list curpos (and prop t)))))

(defun slime-inspector-next-inspectable-object (arg)
  "Move point to the next inspectable object.
With optional ARG, move across that many objects.
If ARG is negative, move backwards."
  (interactive "p")
  (let ((maxpos (point-max)) (minpos (point-min))
        (previously-wrapped-p nil))
    ;; Forward.
    (while (> arg 0)
      (destructuring-bind (pos foundp)
          (slime-find-inspectable-object 'next maxpos)
        (if foundp
            (progn (goto-char pos) (setq arg (1- arg))
                   (setq previously-wrapped-p nil))
            (if (not previously-wrapped-p) ; cycle detection
                (progn (goto-char minpos) (setq previously-wrapped-p t))
                (error "No inspectable objects")))))
    ;; Backward.
    (while (< arg 0)
      (destructuring-bind (pos foundp)
          (slime-find-inspectable-object 'prev minpos)
        ;; SLIME-OPEN-INSPECTOR inserts the title of an inspector page
        ;; as a presentation at the beginning of the buffer; skip
        ;; that.  (Notice how this problem can not arise in ``Forward.'')
        (if (and foundp (/= pos minpos))
            (progn (goto-char pos) (setq arg (1+ arg))
                   (setq previously-wrapped-p nil))
            (if (not previously-wrapped-p) ; cycle detection
                (progn (goto-char maxpos) (setq previously-wrapped-p t))
                (error "No inspectable objects")))))))

(defun slime-inspector-previous-inspectable-object (arg)
  "Move point to the previous inspectable object.
With optional ARG, move across that many objects.
If ARG is negative, move forwards."
  (interactive "p")
  (slime-inspector-next-inspectable-object (- arg)))
  
(defun slime-inspector-describe ()
  (interactive)
  (slime-eval-describe `(swank:describe-inspectee)))

(defun slime-inspector-pprint (part)
  (interactive (list (or (get-text-property (point) 'slime-part-number)
                         (error "No part at point"))))
  (slime-eval-describe `(swank:pprint-inspector-part ,part)))

(defun slime-inspector-show-source (part)
  (interactive (list (or (get-text-property (point) 'slime-part-number)
                         (error "No part at point"))))
  (slime-eval-async 
   `(swank:find-source-location-for-emacs '(:inspector ,part))
   #'slime-show-source-location))
  
(defun slime-inspector-reinspect ()
  (interactive)
  (slime-eval-async `(swank:inspector-reinspect)
                    (lexical-let ((point (slime-inspector-position)))
                      (lambda (parts)
                        (slime-open-inspector parts point)))))

(defun slime-inspector-insert-more-button (index previous)
  (slime-insert-propertized 
   (list 'slime-range-button (list index previous)
         'mouse-face 'highlight
         'face 'slime-inspector-action-face)
   (if previous " [--more--]\n" " [--more--]")))

(defun slime-inspector-fetch-more (button)
  (destructuring-bind (index prev) button
    (slime-inspector-fetch-chunk 
     (list '() (1+ index) index index) prev
     (slime-rcurry 
      (lambda (chunk prev)
        (let ((inhibit-read-only t))
          (apply #'delete-region (slime-property-bounds 'slime-range-button))
          (slime-inspector-insert-chunk chunk prev (not prev))))
      prev))))

(defun slime-inspector-fetch-chunk (chunk prev cont)
  (slime-inspector-fetch chunk slime-inspector-limit prev cont))

(defun slime-inspector-fetch (chunk limit prev cont)
  (destructuring-bind (from to) (slime-inspector-next-range chunk limit prev)
    (cond ((and from to)
           (slime-eval-async 
            `(swank:inspector-range ,from ,to)
            (slime-rcurry (lambda (chunk2 chunk1 limit prev cont)
                            (slime-inspector-fetch 
                             (slime-inspector-join-chunks chunk1 chunk2)
                             limit prev cont))
                          chunk limit prev cont)))
          (t (funcall cont chunk)))))

(defun slime-inspector-next-range (chunk limit prev)
  (destructuring-bind (_ len start end) chunk
    (let ((count (- end start)))
      (cond ((and prev (< 0 start) (or (not limit) (< count limit)))
             (list (if limit (max (- end limit) 0) 0) start))
            ((and (not prev) (< end len) (or (not limit) (< count limit)))
             (list end (if limit (+ start limit) most-positive-fixnum)))
            (t '(nil nil))))))

(defun slime-inspector-join-chunks (chunk1 chunk2)
  (destructuring-bind (i1 l1 s1 e1) chunk1
    (destructuring-bind (i2 l2 s2 e2) chunk2
      (cond ((= e1 s2)
             (list (append i1 i2) l2 s1 e2))
            ((= e2 s1)
             (list (append i2 i1) l2 s2 e1))
            (t (error "Invalid chunks"))))))

(slime-define-keys slime-inspector-mode-map
  ([return] 'slime-inspector-operate-on-point)
  ((kbd "M-RET") 'slime-inspector-copy-down)
  ("\C-m"   'slime-inspector-operate-on-point)
  ([mouse-2] 'slime-inspector-operate-on-click)
  ("l" 'slime-inspector-pop)
  ("n" 'slime-inspector-next)
  (" " 'slime-inspector-next)
  ("d" 'slime-inspector-describe)
  ("p" 'slime-inspector-pprint)
  ("q" 'slime-inspector-quit)
  ("g" 'slime-inspector-reinspect)
  ("\C-i" 'slime-inspector-next-inspectable-object)
  ([(shift tab)] 'slime-inspector-previous-inspectable-object) ; Emacs translates S-TAB
  ([backtab]     'slime-inspector-previous-inspectable-object) ; to BACKTAB on X.
  ("\M-." 'slime-edit-definition)
  ("." 'slime-inspector-show-source)
  (slime-prefix-key slime-prefix-map))


;;;; Buffer selector

(defvar slime-selector-methods nil
  "List of buffer-selection methods for the `slime-select' command.
Each element is a list (KEY DESCRIPTION FUNCTION).
DESCRIPTION is a one-line description of what the key selects.")

(defun slime-selector ()
  "Select a new buffer by type, indicated by a single character.
The user is prompted for a single character indicating the method by
which to choose a new buffer. The `?' character describes the
available methods.

See `def-slime-selector-method' for defining new methods."
  (interactive)
  (message "Select [%s]: " 
           (apply #'string (mapcar #'car slime-selector-methods)))
  (let* ((ch (save-window-excursion
               (select-window (minibuffer-window))
               (read-char)))
         (method (find ch slime-selector-methods :key #'car)))
    (cond ((null method)
           (message "No method for character: ?\\%c" ch)
           (ding)
           (sleep-for 1)
           (discard-input)
           (slime-selector))
          (t
           (funcall (third method))))))

(defmacro def-slime-selector-method (key description &rest body)
  "Define a new `slime-select' buffer selection method.

KEY is the key the user will enter to choose this method.

DESCRIPTION is a one-line sentence describing how the method
selects a buffer.

BODY is a series of forms which are evaluated when the selector
is chosen. The returned buffer is selected with
switch-to-buffer."
  `(setq slime-selector-methods
         (sort* (cons (list ,key ,description
                            (lambda () 
                              (let ((buffer (progn ,@body)))
                                (cond ((get-buffer buffer)
                                       (switch-to-buffer buffer))
                                      (t
                                       (message "No such buffer: %S" buffer)
                                       (ding))))))
                      (remove* ,key slime-selector-methods :key #'car))
                #'< :key #'car)))

(def-slime-selector-method ?? "Selector help buffer."
  (ignore-errors (kill-buffer "*Select Help*"))
  (with-current-buffer (get-buffer-create "*Select Help*")
    (insert "Select Methods:\n\n")
    (loop for (key line function) in slime-selector-methods
          do (insert (format "%c:\t%s\n" key line)))
    (help-mode)
    (display-buffer (current-buffer) t)
    (shrink-window-if-larger-than-buffer 
     (get-buffer-window (current-buffer))))
  (slime-selector)
  (current-buffer))

(def-slime-selector-method ?r
  "SLIME Read-Eval-Print-Loop."
  (slime-output-buffer))

(def-slime-selector-method ?i
  "*inferior-lisp* buffer."
  (cond ((and (slime-connected-p) (slime-process))
         (process-buffer (slime-process)))
        (t
         "*inferior-lisp*")))

(def-slime-selector-method ?v
  "*slime-events* buffer."
  slime-event-buffer-name)

(def-slime-selector-method ?l
  "most recently visited lisp-mode buffer."
  (slime-recently-visited-buffer 'lisp-mode))

(def-slime-selector-method ?d
  "*sldb* buffer for the current connection."
  (or (sldb-get-default-buffer)
      (error "No debugger buffer")))

(def-slime-selector-method ?e
  "most recently visited emacs-lisp-mode buffer."
  (slime-recently-visited-buffer 'emacs-lisp-mode))

(def-slime-selector-method ?c
  "SLIME connections buffer."
  (slime-list-connections)
  slime-connections-buffer-name)

(def-slime-selector-method ?t
  "SLIME threads buffer."
  (slime-list-threads)
  slime-threads-buffer-name)

(defun slime-recently-visited-buffer (mode)
  "Return the most recently visited buffer whose major-mode is MODE.
Only considers buffers that are not already visible."
  (loop for buffer in (buffer-list)
        when (and (with-current-buffer buffer (eq major-mode mode))
                  (not (string-match "^ " (buffer-name buffer)))
                  (null (get-buffer-window buffer 'visible)))
        return buffer
        finally (error "Can't find unshown buffer in %S" mode)))


;;;; Editing commands



;;;; Font Lock

(defcustom slime-highlight-suppressed-forms t
  "Display forms disabled by reader conditionals as comments."
  :type '(choice (const :tag "Enable" t) (const :tag "Disable" nil))
  :group 'slime-mode)

(defface slime-reader-conditional-face
  (if (slime-face-inheritance-possible-p)
    '((t (:inherit font-lock-comment-face)))
  '((((background light)) (:foreground "DimGray" :bold t))
    (((background dark)) (:foreground "LightGray" :bold t))))
  "Face for compiler notes while selected."
  :group 'slime-mode-faces)

(defun slime-search-suppressed-forms (limit)
  "Find reader conditionalized forms where the test is false."
  (when (and slime-highlight-suppressed-forms
             (slime-connected-p)
	     (re-search-forward "^\\([^;\n]*?[ \t(]\\)?#[-+]" limit t))
    (ignore-errors
      (let* ((start (- (point) 2))
             (char (char-before))
             (e (read (current-buffer)))
             (val (slime-eval-feature-conditional e)))
        (when (<= (point) limit)
          (if (or (and (eq char ?+) (not val))
                  (and (eq char ?-) val))
              (progn 
                (forward-sexp) (backward-sexp)
                (slime-forward-sexp)
                (assert (<= (point) limit))
                (let ((md (match-data)))
                  (fill md nil)
                  (setf (first md) start)
                  (setf (second md) (point))
                  (set-match-data md)
                  t))
            (slime-search-suppressed-forms limit)))))))

(defun slime-activate-font-lock-magic ()
  (if (featurep 'xemacs)
      (let ((pattern `((slime-search-suppressed-forms
                        (0 slime-reader-conditional-face t)))))
        (dolist (sym '(lisp-font-lock-keywords
                       lisp-font-lock-keywords-1
                       lisp-font-lock-keywords-2))
          (set sym (append (symbol-value sym) pattern))))
    (font-lock-add-keywords
     'lisp-mode
     `((slime-search-suppressed-forms 0 ,''slime-reader-conditional-face t)))))

(when slime-highlight-suppressed-forms
  (slime-activate-font-lock-magic))


;;;; Indentation

(defun slime-update-indentation ()
  "Update indentation for all macros defined in the Lisp system."
  (interactive)
  (slime-eval-async '(swank:update-indentation-information)))

(defvar slime-indentation-update-hooks)

(defun slime-handle-indentation-update (alist)
  "Update Lisp indent information.

ALIST is a list of (SYMBOL-NAME . INDENT-SPEC) of proposed indentation
settings for `common-lisp-indent-function'. The appropriate property
is setup, unless the user already set one explicitly."
  (dolist (info alist)
    (let ((symbol (intern (car info)))
          (indent (cdr info)))
      ;; Does the symbol have an indentation value that we set?
      (when (equal (get symbol 'common-lisp-indent-function)
                   (get symbol 'slime-indent))
        (put symbol 'slime-indent indent)
        (put symbol 'common-lisp-indent-function indent))
      (run-hook-with-args 'slime-indentation-update-hooks symbol indent))))


;;;; Contrib modules

(defvar slime-required-modules '())

(defun slime-require (module)
  (assert (keywordp module))
  (pushnew module slime-required-modules)
  (when (slime-connected-p)
    (slime-load-contribs)))

(defun slime-load-contribs ()
  (let ((needed (remove-if (lambda (s) 
                             (member (subseq (symbol-name s) 1)
                                     (mapcar #'downcase (slime-lisp-modules))))
                           slime-required-modules)))
    (when needed
      (slime-eval-async `(swank:swank-require ',needed)
                        (lambda (new-modules)
                          (setf (slime-lisp-modules) new-modules))))))


;;;;; Pull-down menu

(defvar slime-easy-menu
  (let ((C '(slime-connected-p)))
    `("SLIME"
      [ "Edit Definition..."       slime-edit-definition ,C ]
      [ "Return From Definition"   slime-pop-find-definition-stack ,C ]
      [ "Complete Symbol"          slime-complete-symbol ,C ]
      [ "Show REPL"                slime-switch-to-output-buffer ,C ]
      "--"
      ("Evaluation"
       [ "Eval Defun"              slime-eval-defun ,C ]
       [ "Eval Last Expression"    slime-eval-last-expression ,C ]
       [ "Eval And Pretty-Print"   slime-pprint-eval-last-expression ,C ]
       [ "Eval Region"             slime-eval-region ,C ]
       [ "Interactive Eval..."     slime-interactive-eval ,C ]
       [ "Edit Lisp Value..."      slime-edit-value ,C ]
       [ "Call Defun"              slime-call-defun ,C ])
      ("Debugging"
       [ "Macroexpand Once..."     slime-macroexpand-1 ,C ]
       [ "Macroexpand All..."      slime-macroexpand-all ,C ]
       [ "Create Trace Buffer"     slime-redirect-trace-output ,C ]
       [ "Toggle Trace..."         slime-toggle-trace-fdefinition ,C ]
       [ "Untrace All"             slime-untrace-all ,C]
       [ "Disassemble..."          slime-disassemble-symbol ,C ]
       [ "Inspect..."              slime-inspect ,C ])
      ("Compilation"
       [ "Compile Defun"           slime-compile-defun ,C ]
       [ "Compile/Load File"       slime-compile-and-load-file ,C ]
       [ "Compile File"            slime-compile-file ,C ]
       [ "Compile Region"          slime-compile-region ,C ]
       "--"
       [ "Next Note"               slime-next-note t ]
       [ "Previous Note"           slime-previous-note t ]
       [ "Remove Notes"            slime-remove-notes t ]
       [ "List Notes"              slime-list-compiler-notes ,C ])
      ("Cross Reference"
       [ "Who Calls..."            slime-who-calls ,C ]
       [ "Who References... "      slime-who-references ,C ]
       [ "Who Sets..."             slime-who-sets ,C ]
       [ "Who Binds..."            slime-who-binds ,C ]
       [ "Who Macroexpands..."     slime-who-macroexpands ,C ]
       [ "Who Specializes..."      slime-who-specializes ,C ]
       [ "List Callers..."         slime-list-callers ,C ]
       [ "List Callees..."         slime-list-callees ,C ]
       [ "Next Location"           slime-next-location t ])
      ("Editing"
       [ "Check Parens"            check-parens t]
       [ "Update Indentation"      slime-update-indentation ,C]
       [ "Select Buffer"           slime-selector t])
      ("Profiling"
       [ "Toggle Profiling..."     slime-toggle-profile-fdefinition ,C ]
       [ "Profile Package"         slime-profile-package ,C]
       [ "Unprofile All"           slime-unprofile-all ,C ]
       [ "Show Profiled"           slime-profiled-functions ,C ]
       "--"
       [ "Report"                  slime-profile-report ,C ]
       [ "Reset Counters"          slime-profile-reset ,C ])
      ("Documentation"
       [ "Describe Symbol..."      slime-describe-symbol ,C ]
       [ "Apropos..."              slime-apropos ,C ]
       [ "Apropos all..."          slime-apropos-all ,C ]
       [ "Apropos Package..."      slime-apropos-package ,C ]
       [ "Hyperspec..."            slime-hyperspec-lookup t ])
      "--"
      [ "Interrupt Command"        slime-interrupt ,C ]
      [ "Abort Async. Command"     slime-quit ,C ]
      [ "Sync Package & Directory" slime-sync-package-and-default-directory ,C]
      [ "Set Package in REPL"      slime-repl-set-package ,C])))

(defvar slime-repl-easy-menu
  (let ((C '(slime-connected-p)))
    `("REPL"
      [ "Send Input"             slime-repl-return ,C ]
      [ "Close and Send Input "  slime-repl-closing-return ,C ]
      [ "Interrupt Lisp process" slime-interrupt ,C ]
      "--"
      [ "Previous Input"         slime-repl-previous-input t ]
      [ "Next Input"             slime-repl-next-input t ]
      [ "Goto Previous Prompt "  slime-repl-previous-prompt t ]
      [ "Goto Next Prompt "      slime-repl-next-prompt t ]
      [ "Clear Last Output"      slime-repl-clear-output t ]
      [ "Clear Buffer "          slime-repl-clear-buffer t ]
      [ "Kill Current Input"     slime-repl-kill-input t ])))
      
(defvar slime-sldb-easy-menu
  (let ((C '(slime-connected-p)))
    `("SLDB"
      [ "Next Frame" sldb-down t ]
      [ "Previous Frame" sldb-up t ]
      [ "Toggle Frame Details" sldb-toggle-details t ]
      [ "Next Frame (Details)" sldb-details-down t ]
      [ "Previous Frame (Details)" sldb-details-up t ]
      "--"
      [ "Eval Expression..." slime-interactive-eval ,C ]
      [ "Eval in Frame..." sldb-eval-in-frame ,C ]
      [ "Eval in Frame (pretty print)..." sldb-pprint-eval-in-frame ,C ]
      [ "Inspect In Frame..." sldb-inspect-in-frame ,C ]
      [ "Inspect Condition Object" sldb-inspect-condition ,C ]
      [ "Print Condition to REPL" sldb-print-condition t ]
      "--"
      [ "Restart Frame" sldb-restart-frame ,C ]
      [ "Return from Frame..." sldb-return-from-frame ,C ]
      ("Invoke Restart"
       [ "Continue" sldb-continue ,C ]
       [ "Abort"    sldb-abort ,C ]
       [ "Step"      sldb-step ,C ]
       [ "Step next" sldb-next ,C ]
       [ "Step out"  sldb-out ,C ]
       )
      "--"
      [ "Quit (throw)" sldb-quit ,C ]
      [ "Break With Default Debugger" sldb-break-with-default-debugger ,C ])))

(easy-menu-define menubar-slime slime-mode-map "SLIME" slime-easy-menu)

(defun slime-add-easy-menu ()
  (easy-menu-add slime-easy-menu 'slime-mode-map))

(add-hook 'slime-mode-hook 'slime-add-easy-menu)

(defun slime-repl-add-easy-menu ()
  (easy-menu-define menubar-slime-repl slime-repl-mode-map
    "REPL" slime-repl-easy-menu)
  (easy-menu-define menubar-slime slime-repl-mode-map 
    "SLIME" slime-easy-menu)
  (easy-menu-add slime-repl-easy-menu 'slime-repl-mode-map))

(add-hook 'slime-repl-mode-hook 'slime-repl-add-easy-menu)

(defun slime-sldb-add-easy-menu ()
  (easy-menu-define menubar-slime-sldb 
    sldb-mode-map "SLDB" slime-sldb-easy-menu)
  (easy-menu-add slime-sldb-easy-menu 'sldb-mode-map))

(add-hook 'sldb-mode-hook 'slime-sldb-add-easy-menu)


;;;; Cheat Sheet

(defvar slime-cheat-sheet-table
  '((:title "Editing lisp code"
     :map slime-mode-map
     :bindings ((slime-eval-defun "Evaluate current top level form")
                (slime-compile-defun "Compile current top level form")
                (slime-interactive-eval "Prompt for form and eval it")
                (slime-compile-and-load-file "Compile and load current file")
                (slime-sync-package-and-default-directory "Synch default package and directory with current buffer")
                (slime-next-note "Next compiler note")
                (slime-previous-note "Previous compiler note")
                (slime-remove-notes "Remove notes")
                slime-hyperspec-lookup))
    (:title "Completion"
     :map slime-mode-map
     :bindings (slime-indent-and-complete-symbol
                slime-fuzzy-complete-symbol))
    (:title "At the REPL" 
     :map slime-repl-mode-map
     :bindings (slime-repl-clear-buffer
                slime-describe-symbol))
    (:title "Within SLDB buffers" 
     :map sldb-mode-map
     :bindings ((sldb-default-action "Do 'whatever' with thing at point")
                (sldb-toggle-details "Toggle frame details visualization")
                (sldb-quit "Quit to REPL")
                (sldb-abort "Invoke ABORT restart")
                (sldb-continue "Invoke CONTINUE restart (if available)")
                (sldb-show-source "Jump to frame's source code")
                (sldb-eval-in-frame "Evaluate in frame at point")
                (sldb-inspect-in-frame "Evaluate in frame at point and inspect result")))
    (:title "Within the Inspector" 
     :map slime-inspector-mode-map
     :bindings ((slime-inspector-next-inspectable-object "Jump to next inspectable object")
                (slime-inspector-operate-on-point "Inspect object or execute action at point")
                (slime-inspector-reinspect "Reinspect current object")
                (slime-inspector-pop "Return to previous object")
                (slime-inspector-copy-down "Send object at point to REPL")
                (slime-inspector-quit "Quit")))
    (:title "Finding Definitions"
     :map slime-mode-map
     :bindings (slime-edit-definition
                slime-pop-find-definition-stack))))

(defun slime-cheat-sheet ()
  (interactive)
  (switch-to-buffer-other-frame (get-buffer-create "*SLIME Cheat Sheet*"))
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))
  (goto-char (point-min))
  (insert "SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode).\n\n")
  (dolist (mode slime-cheat-sheet-table)
    (let ((title (getf mode :title))
          (mode-map (getf mode :map))
          (mode-keys (getf mode :bindings)))
      (insert title)
      (insert ":\n")
      (insert (make-string (1+ (length title)) ?-))
      (insert "\n")
      (let ((keys '())
            (descriptions '()))
        (dolist (func mode-keys)
          ;; func is eithor the function name or a list (NAME DESCRIPTION)
          (push (if (symbolp func)
                    (prin1-to-string func)
                    (second func))
                descriptions)
          (let ((all-bindings (where-is-internal (if (symbolp func)
                                                     func
                                                     (first func))
                                                 (symbol-value mode-map)))
                (key-bindings '()))
            (dolist (binding all-bindings)
              (when (and (vectorp binding)
                         (integerp (aref binding 0)))
                (push binding key-bindings)))
            (push (mapconcat 'key-description key-bindings " or ") keys)))
        (loop
           with key-length = (apply 'max (mapcar 'length keys))
           with desc-length = (apply 'max (mapcar 'length descriptions))
           for key in (nreverse keys)
           for desc in (nreverse descriptions)
           do (insert desc)
           do (insert (make-string (- desc-length (length desc)) ? ))
           do (insert " => ")
           do (insert (if (string= "" key)
                          "<not on any key>"
                          key))
           do (insert "\n")
           finally do (insert "\n")))))
  (setq buffer-read-only t)
  (goto-char (point-min)))


;;;; Test suite

(defstruct (slime-test (:conc-name slime-test.))
  name fname args doc inputs fails-for)
  
(defvar slime-tests '()
  "Names of test functions.")

(defvar slime-test-debug-on-error nil
  "*When non-nil debug errors in test cases.")

(defvar slime-total-tests nil
  "Total number of tests executed during a test run.")

(defvar slime-failed-tests nil
  "Total number of failed tests during a test run.")

(defvar slime-expected-failures nil
  "Total number of expected failures during a test run")

(defvar slime-test-buffer-name "*Tests*"
  "The name of the buffer used to display test results.")

(defvar slime-lisp-under-test nil
  "The name of Lisp currently executing the tests.")

(defvar slime-randomize-test-order t
  "*If t execute tests in random order.
If nil, execute them in definition order.")

;; dynamically bound during a single test
(defvar slime-current-test)
(defvar slime-unexpected-failures)


;;;;; Execution engine

(defun slime-run-tests ()
  "Run the test suite.
The results are presented in an outline-mode buffer, with the tests
that succeeded initially folded away."
  (interactive)
  (assert (slime-at-top-level-p) () "Pending RPCs or open debuggers.")
  (slime-create-test-results-buffer)
  (unwind-protect
      (let ((slime-repl-history-file 
             (expand-file-name "slime-repl-history" (slime-temp-directory)))
            (slime-tests (if slime-randomize-test-order
                             (slime-shuffle-list slime-tests)
                           slime-tests)))
        (slime-execute-tests))
    (pop-to-buffer slime-test-buffer-name)
    (goto-char (point-min))
    (hide-body)
    ;; Expose failed tests
    (dolist (o (overlays-in (point-min) (point-max)))
      (when (overlay-get o 'slime-failed-test)
        (goto-char (overlay-start o))
        (show-subtree)))))

(defun slime-run-one-test (name)
  "Ask for the name of a test and then execute the test."
  (interactive (list (slime-read-test-name)))
  (let ((test (find name slime-tests :key #'slime-test.name)))
    (assert test)
    (let ((slime-tests (list test)))
      (slime-run-tests))))

(defun slime-read-test-name ()
  (let ((alist (mapcar (lambda (test) 
                         (list (symbol-name (slime-test.name test))))
                       slime-tests)))
    (read (completing-read "Test: " alist nil t))))

(defun slime-test-should-fail-p ()
  (member slime-lisp-under-test (slime-test.fails-for slime-current-test)))

(defun slime-shuffle-list (list)
  (let* ((len (length list))
         (taken (make-vector len nil))
         (result (make-vector len nil)))
    (dolist (e list)
      (while (let ((i (random len)))
               (cond ((aref taken i))
                     (t (aset taken i t)
                        (aset result i e)
                        nil)))))
    (append result '())))

(defun slime-execute-tests ()
  "Execute each test case with each input.
Return the number of failed tests."
  (save-window-excursion
    (let ((slime-total-tests 0)
          (slime-expected-passes 0)
          (slime-unexpected-failures 0)
          (slime-expected-failures 0)
          (slime-lisp-under-test (slime-lisp-implementation-name)))
      (dolist (slime-current-test slime-tests)
        (with-struct (slime-test. name (function fname) inputs) 
            slime-current-test
          (slime-test-heading 1 "%s" name)
          (dolist (input inputs)
            (incf slime-total-tests)
            (message "%s: %s" name input)
            (slime-test-heading 2 "input: %s" input)
            (if slime-test-debug-on-error
                (let ((debug-on-error t)
                      (debug-on-quit t))
                  (catch 'skip
                    (apply function input)))
              (condition-case err
                  (apply function input)
                (error
                 (cond ((slime-test-should-fail-p)
                        (incf slime-expected-failures)
                        (slime-test-failure "ERROR (expected)"
                                            (format "%S" err)))
                       (t
                        (incf slime-unexpected-failures)
                        (slime-print-check-error err)))))))))
      (let ((summary 
             (cond ((and (zerop slime-expected-failures)
                         (zerop slime-unexpected-failures))
                    (format "All %S tests completed successfully."
                            slime-total-tests))
                   (t
                    (format "Failed on %S (%S expected) of %S tests."
                            (+ slime-expected-failures
                               slime-unexpected-failures)
                            slime-expected-failures
                            slime-total-tests)))))
        (save-excursion
          (with-current-buffer slime-test-buffer-name
            (goto-char (point-min))
            (insert summary "\n\n")))
        (message "%s" summary)
        slime-unexpected-failures))))

(defun slime-batch-test (results-file &optional test-name randomize)
  "Run the test suite in batch-mode.
Exits Emacs when finished. The exit code is the number of failed tests."
  (let ((slime-test-debug-on-error nil))
    (slime)
    ;; Block until we are up and running.
    (let* ((timeout 30)
           (cell (cons nil nil))
           (timer (run-with-timer timeout nil (lambda (cell) 
                                                (setcar cell t))
                                  cell)))
      (while (not (slime-connected-p))
        (sit-for 1)
        (when (car cell)
          (with-temp-file results-file 
            (insert (format "TIMEOUT: Failed to connect within %s seconds."
                            timeout)))
          (kill-emacs 252))))
    (slime-sync-to-top-level 5)
    (switch-to-buffer "*scratch*")
    (let* ((slime-randomize-test-order (when randomize (random t) t))
           (failed-tests (cond (test-name (slime-run-one-test test-name))
                               (t (slime-run-tests)))))
      (with-current-buffer slime-test-buffer-name
        (slime-delete-hidden-outline-text)
        (goto-char (point-min))
        (insert "-*- outline -*-\n\n")
        (write-file results-file))
      (kill-emacs failed-tests))))


;;;;; Results buffer creation and output

(defun slime-create-test-results-buffer ()
  "Create and initialize the buffer for test suite results."
  (ignore-errors (kill-buffer slime-test-buffer-name))
  (with-current-buffer (get-buffer-create slime-test-buffer-name)
    (erase-buffer)
    (outline-mode)
    (set (make-local-variable 'outline-regexp) "\\*+")
    (slime-set-truncate-lines)))

(defun slime-delete-hidden-outline-text ()
  "Delete the hidden parts of an outline-mode buffer."
  (loop do (when (eq (get-char-property (point) 'invisible) 'outline)
             (delete-region (point)
                            (next-single-char-property-change (point)
                                                              'invisible)))
        until (eobp)
        do (goto-char (next-single-char-property-change (point) 'invisible))))

(defun slime-test-heading (level format &rest args)
  "Output a test suite heading.
LEVEL gives the depth of nesting: 1 for top-level, 2 for a subheading, etc."
  (with-current-buffer slime-test-buffer-name
    (goto-char (point-max))
    (insert (make-string level ?*)
            " "
            (apply 'format format args)
            "\n")))

(defun slime-test-failure (keyword string)
  "Output a failure message from the test suite.
KEYWORD names the type of failure and STRING describes the reason."
  (with-current-buffer slime-test-buffer-name
    (goto-char (point-max))
    (let ((start (point)))
      (insert keyword ": ")
      (let ((overlay (make-overlay start (point))))
        (overlay-put overlay 'slime-failed-test t)
        (overlay-put overlay 'face 'bold)))
    (insert string "\n")))

(defun slime-test-message (string)
  "Output a message from the test suite."
  (with-current-buffer slime-test-buffer-name
    (goto-char (point-max))
    (insert string "\n")))


;;;;; Macros for defining test cases

(defmacro def-slime-test (name args doc inputs &rest body)
  "Define a test case.
NAME ::= SYMBOL | (SYMBOL (FAILS-FOR*)) is a symbol naming the test.
ARGS is a lambda-list.
DOC is a docstring.
INPUTS is a list of argument lists, each tested separately.
BODY is the test case. The body can use `slime-check' to test
conditions (assertions)."
  (multiple-value-bind (name fails-for) (etypecase name
                                          (symbol (values name '()))
                                          (cons name))
    (let ((fname (intern (format "slime-test-%s" name))))
      `(progn
         (defun ,fname ,args
           ,doc
           (slime-sync-to-top-level 0.3)
           ,@body
           (slime-sync-to-top-level 0.3))
         (setq slime-tests 
               (append (remove* ',name slime-tests :key 'slime-test.name)
                       (list (make-slime-test :name ',name :fname ',fname
                                              :fails-for ',fails-for
                                              :inputs ,inputs))))))))

(put 'def-slime-test 'lisp-indent-function 4)

(defmacro slime-check (test-name &rest body)
  "Check a condition (assertion.)
TEST-NAME can be a symbol, a string, or a (FORMAT-STRING . ARGS) list.
BODY returns true if the check succeeds."
  (let ((check-name (gensym "check-name-")))
    `(let ((,check-name ,(typecase test-name
                           (symbol (symbol-name test-name))
                           (string test-name)
                           (cons `(format ,@test-name)))))
       (if (progn ,@body)
           (slime-print-check-ok ,check-name)
         (cond ((slime-test-should-fail-p)
                (incf slime-expected-failures)
                (slime-test-failure "FAIL (expected)" ,check-name))
               (t
                (incf slime-unexpected-failures)
                (slime-print-check-failed ,check-name)))
         (when slime-test-debug-on-error
           (debug (format "Check failed: %S" ,check-name)))))))

(defun slime-print-check-ok (test-name)
  (slime-test-message (concat "OK: " test-name)))

(defun slime-print-check-failed (test-name)
  (slime-test-failure "FAILED" test-name))

(defun slime-print-check-error (reason)
  (slime-test-failure "ERROR" (format "%S" reason)))

(put 'slime-check 'lisp-indent-function 1)


;;;;; Test case definitions

;; Clear out old tests.
(setq slime-tests nil)

(defun slime-check-top-level (&optional test-name)
  (slime-accept-process-output nil 0.001)
  (slime-check "At the top level (no debugging or pending RPCs)"
    (slime-at-top-level-p)))

(defun slime-at-top-level-p ()
  (and (not (sldb-get-default-buffer))
       (null (slime-rex-continuations))))

(defun slime-wait-condition (name predicate timeout)
  (let ((end (time-add (current-time) (seconds-to-time timeout))))
    (while (not (funcall predicate))
      (let ((now (current-time)))
        (message "waiting for condition: %s [%s.%06d]" name
                 (format-time-string "%H:%M:%S" now) (third now)))
      (cond ((time-less-p end (current-time))
             (error "Timeout waiting for condition: %S" name))
            (t
             ;; XXX if a process-filter enters a recursive-edit, we
             ;; hang forever
             (save-excursion
               (slime-accept-process-output nil 0.1)))))))

(defun slime-sync-to-top-level (timeout)
  (slime-wait-condition "top-level" #'slime-at-top-level-p timeout))

;; XXX: unused function
(defun slime-check-sldb-level (expected)
  (let ((sldb-level (when-let (sldb (sldb-get-default-buffer))
                      (with-current-buffer sldb
                        sldb-level))))
    (slime-check ("SLDB level (%S) is %S" expected sldb-level)
      (equal expected sldb-level))))

(defun slime-test-expect (name expected actual &optional test)
  (when (stringp expected) (setq expected (substring-no-properties expected)))
  (when (stringp actual)   (setq actual (substring-no-properties actual)))
  (slime-check ("%s:\nexpected: [%S]\n  actual: [%S]" name expected actual)
    (funcall (or test #'equal) expected actual)))

(defun sldb-level ()
  (when-let (sldb (sldb-get-default-buffer))
    (with-current-buffer sldb
      sldb-level)))

(defun slime-sldb-level= (level)
  (equal level (sldb-level)))

(def-slime-test narrowing ()
  "Check that narrowing is properly sustained."
  '(())
  (slime-check-top-level)
  (let ((random-buffer-name (symbol-name (gensym)))
        (defun-pos) (tmpbuffer))
    (with-temp-buffer
      (dotimes (i 100) (insert (format ";;; %d. line\n" i)))
      (setq tmpbuffer (current-buffer))
      (setq defun-pos (point))
      (insert (concat "(defun __foo__ (x y)" "\n"
                      "  'nothing)"          "\n"))
      (dotimes (i 100) (insert (format ";;; %d. line\n" (+ 100 i))))
      (slime-check "Checking that newly created buffer is not narrowed."
        (not (slime-buffer-narrowed-p)))

      (goto-char defun-pos)
      (narrow-to-defun)
      (slime-check "Checking that narrowing succeeded."
       (slime-buffer-narrowed-p))

      (slime-with-popup-buffer (random-buffer-name)
        (slime-check ("Checking that we're in Slime's temp buffer `%s'" random-buffer-name)
          (equal (buffer-name (current-buffer)) random-buffer-name)))
      (with-current-buffer random-buffer-name
        ;; Notice that we cannot quit the buffer within the the extent
        ;; of slime-with-output-to-temp-buffer.
        (slime-popup-buffer-quit t)) 
      (slime-check ("Checking that we've got back from `%s'" random-buffer-name)
        (and (eq (current-buffer) tmpbuffer)
             (= (point) defun-pos)))
      
      (slime-check "Checking that narrowing sustained after quitting Slime's temp buffer."
        (slime-buffer-narrowed-p))

      (let ((slime-buffer-package "SWANK")
            (symbol '*buffer-package*))
        (slime-edit-definition (symbol-name symbol))
        (slime-check ("Checking that we've got M-. into swank.lisp. %S" symbol)
          (string= (file-name-nondirectory (buffer-file-name))
                   "swank.lisp"))
        (slime-pop-find-definition-stack)
        (slime-check ("Checking that we've got back.")
          (and (eq (current-buffer) tmpbuffer)
               (= (point) defun-pos)))

        (slime-check "Checking that narrowing sustained after M-,"
        (slime-buffer-narrowed-p)))
      )) 
  (slime-check-top-level))

(def-slime-test find-definition
    (name buffer-package snippet)
    "Find the definition of a function or macro in swank.lisp."
    '(("start-server" "SWANK" "(defun start-server ")
      ("swank::start-server" "CL-USER" "(defun start-server ")
      ("swank:start-server" "CL-USER" "(defun start-server "))
  (switch-to-buffer "*scratch*")        ; not buffer of definition
  (slime-check-top-level)
  (let ((orig-buffer (current-buffer))
        (orig-pos (point))
        (enable-local-variables nil)    ; don't get stuck on -*- eval: -*-
        (slime-buffer-package buffer-package))
    (slime-edit-definition name)
    ;; Postconditions
    (slime-check ("Definition of `%S' is in swank.lisp." name)
      (string= (file-name-nondirectory (buffer-file-name)) "swank.lisp"))
    (slime-check "Definition now at point." (looking-at snippet))
    (slime-pop-find-definition-stack)
    (slime-check "Returning from definition restores original buffer/position."
      (and (eq orig-buffer (current-buffer))
           (= orig-pos (point)))))
    (slime-check-top-level))

(def-slime-test find-definition.2
    (buffer-content buffer-package snippet)
    "Check that we're able to find definitions even when
confronted with nasty #.-fu."
    '(("#.(prog1 nil (defvar *foobar* 42))

       (defun .foo. (x)
         (+ x #.*foobar*))

       #.(prog1 nil (makunbound '*foobar*))
       "
       "SWANK"
       "(defun .foo. "
       ))
  (let ((slime-buffer-package buffer-package))
    (with-temp-buffer
      (insert buffer-content)
      (slime-check-top-level)
      (slime-eval 
       `(swank:compile-string-for-emacs
         ,buffer-content
         ,(buffer-name)
         ,0
         ,nil
         ,nil))
      (let ((bufname (buffer-name)))
        (slime-edit-definition ".foo.")
        (slime-check ("Definition of `.foo.' is in buffer `%s'." bufname)
          (string= (buffer-name) bufname))
        (slime-check "Definition now at point." (looking-at snippet)))
      )))

(def-slime-test complete-symbol
    (prefix expected-completions)
    "Find the completions of a symbol-name prefix."
    '(("cl:compile" (("cl:compile" "cl:compile-file" "cl:compile-file-pathname"
                      "cl:compiled-function" "cl:compiled-function-p" 
                      "cl:compiler-macro" "cl:compiler-macro-function")
                     "cl:compile"))
      ("cl:foobar" (nil ""))
      ("swank::compile-file" (("swank::compile-file" 
                               "swank::compile-file-for-emacs"
                               "swank::compile-file-if-needed"
                               "swank::compile-file-pathname")
                              "swank::compile-file"))
      ("cl:m-v-l" (nil "")))
  (let ((completions (slime-simple-completions prefix)))
    (slime-test-expect "Completion set" expected-completions completions)))

(def-slime-test arglist
    ;; N.B. Allegro apparently doesn't return the default values of
    ;; optional parameters. Thus the regexp in the start-server
    ;; expected value. In a perfect world we'd find a way to smooth
    ;; over this difference between implementations--perhaps by
    ;; convincing Franz to provide a function that does what we want.
    (function-name expected-arglist)
    "Lookup the argument list for FUNCTION-NAME.
Confirm that EXPECTED-ARGLIST is displayed."
    '(("swank::operator-arglist" "(swank::operator-arglist name package)")
      ("swank::create-socket" "(swank::create-socket host port)")
      ("swank::emacs-connected" "(swank::emacs-connected )")
      ("swank::compile-string-for-emacs"
       "(swank::compile-string-for-emacs string buffer position directory debug)")
      ("swank::connection.socket-io"
       "(swank::connection.socket-io \\(struct\\(ure\\)?\\|object\\|instance\\))")
      ("cl:lisp-implementation-type" "(cl:lisp-implementation-type )")
      ("cl:class-name" 
       "(cl:class-name \\(class\\|object\\|instance\\|structure\\))"))
  (slime-check-top-level)
  (let ((arglist (slime-eval `(swank:operator-arglist ,function-name 
                                                      "swank"))))
    (slime-test-expect "Argument list is as expected"
                       expected-arglist (downcase arglist)
                       #'string-match))
  (slime-check-top-level))

(def-slime-test (compile-defun ("allegro" "lispworks" "clisp"))
    (program subform)
    "Compile PROGRAM containing errors.
Confirm that SUBFORM is correctly located."
    '(("(defun cl-user::foo () (cl-user::bar))" (cl-user::bar))
      ("(defun cl-user::foo () 
          #\\space
          ;;Sdf              
          (cl-user::bar))"
       (cl-user::bar))
      ("(defun cl-user::foo () 
             #+(or)skipped
             #| #||#
                #||# |#
             (cl-user::bar))"
       (cl-user::bar))
      ("(defun cl-user::foo () 
           (list `(1 ,(random 10) 2 ,@(random 10) 3 ,(cl-user::bar))))"
       (cl-user::bar))
      ("(defun cl-user::foo ()
          \"\\\" bla bla \\\"\"
          (cl-user::bar))"
       (cl-user::bar))
      ("(defun cl-user::foo ()
          #.*log-events*
          (cl-user::bar))"
       (cl-user::bar))
      ("#.'(defun x () (/ 1 0))
        (defun foo () 
           (cl-user::bar))
        
        "
       (cl-user::bar))
      ("(defun foo ()
          #+#.'(:and) (/ 1 0))"
       (/ 1 0)))
  (slime-check-top-level)    
  (with-temp-buffer 
    (lisp-mode)
    (insert program)
    (setq slime-buffer-package ":swank")
    (slime-compile-string (buffer-string) 1)
    (setq slime-buffer-package ":cl-user")
    (slime-sync-to-top-level 5)
    (goto-char (point-max))
    (slime-previous-note)
    (slime-check error-location-correct
      (equal (read (current-buffer))
             subform)))
  (slime-check-top-level))

(def-slime-test async-eval-debugging (depth)
  "Test recursive debugging of asynchronous evaluation requests."
  '((1) (2) (3))
  (lexical-let ((depth depth)
                (debug-hook-max-depth 0))
    (let ((debug-hook
           (lambda ()
             (with-current-buffer (sldb-get-default-buffer)
               (when (> sldb-level debug-hook-max-depth)
                 (setq debug-hook-max-depth sldb-level)
                 (if (= sldb-level depth)
                     ;; We're at maximum recursion - time to unwind
                     (sldb-quit)
                   ;; Going down - enter another recursive debug
                   ;; Recursively debug.
                   (slime-eval-async '(error))))))))
      (let ((sldb-hook (cons debug-hook sldb-hook)))
        (slime-eval-async '(error))
        (slime-sync-to-top-level 5)
        (slime-check ("Maximum depth reached (%S) is %S."
                      debug-hook-max-depth depth)
          (= debug-hook-max-depth depth))))))

(def-slime-test unwind-to-previous-sldb-level (level2 level1)
  "Test recursive debugging and returning to lower SLDB levels."
  '((2 1) (4 2))
  (slime-check-top-level)
  (lexical-let ((level2 level2)
                (level1 level1)
                (state 'enter)
                (max-depth 0))
    (let ((debug-hook
           (lambda ()
             (with-current-buffer (sldb-get-default-buffer)
               (setq max-depth (max sldb-level max-depth))
               (ecase state
                 (enter
                  (cond ((= sldb-level level2)
                         (setq state 'leave)
                         (sldb-invoke-restart (sldb-first-abort-restart)))
                        (t
                         (slime-eval-async `(cl:aref cl:nil ,sldb-level)))))
                 (leave
                  (cond ((= sldb-level level1)
                         (setq state 'ok)
                         (sldb-quit))
                        (t
                         (sldb-invoke-restart (sldb-first-abort-restart))
                         ))))))))
      (let ((sldb-hook (cons debug-hook sldb-hook)))
        (slime-eval-async `(cl:aref cl:nil 0))
        (slime-sync-to-top-level 15)
        (slime-check-top-level)
        (slime-check ("Maximum depth reached (%S) is %S." max-depth level2)
          (= max-depth level2))
        (slime-check ("Final state reached.")
          (eq state 'ok))))))

(defun sldb-first-abort-restart ()
  (let ((case-fold-search t))
    (position-if (lambda (x) (string-match "abort" (car x))) sldb-restarts)))

(def-slime-test loop-interrupt-quit
    ()
    "Test interrupting a loop."
    '(())
  (slime-check-top-level)
  (slime-eval-async '(cl:loop) (lambda (_) ) "CL-USER")
  (slime-accept-process-output nil 1)
  (slime-check "In eval state." (slime-busy-p))
  (slime-interrupt)
  (slime-wait-condition "First interrupt" (lambda () (slime-sldb-level= 1)) 5)
  (with-current-buffer (sldb-get-default-buffer) 
    (sldb-quit))
  (slime-sync-to-top-level 5)
  (slime-check-top-level))

(def-slime-test loop-interrupt-continue-interrupt-quit
    ()
    "Test interrupting a previously interrupted but continued loop."
    '(())
  (slime-check-top-level)
  (slime-eval-async '(cl:loop) (lambda (_) ) "CL-USER")
  (sleep-for 1)
  (slime-wait-condition "running" #'slime-busy-p 5)
  (slime-interrupt)
  (slime-wait-condition "First interrupt" (lambda () (slime-sldb-level= 1)) 5)
  (with-current-buffer (sldb-get-default-buffer)
    (sldb-continue))
  (slime-wait-condition "running" (lambda () 
                                    (and (slime-busy-p)
                                         (not (sldb-get-default-buffer)))) 5)
  (slime-interrupt)
  (slime-wait-condition "Second interrupt" (lambda () (slime-sldb-level= 1)) 5)
  (with-current-buffer (sldb-get-default-buffer)
    (sldb-quit))
  (slime-sync-to-top-level 5)
  (slime-check-top-level))
 
(def-slime-test interactive-eval 
    ()
    "Test interactive eval and continuing from the debugger."
    '(())
  (slime-check-top-level)
  (lexical-let ((done nil))
    (let ((sldb-hook (lambda () (sldb-continue) (setq done t))))
      (slime-interactive-eval 
       "(progn(cerror \"foo\" \"restart\")(cerror \"bar\" \"restart\")(+ 1 2))")
      (while (not done) (slime-accept-process-output))
      (slime-sync-to-top-level 5)
      (slime-check-top-level)
      (unless noninteractive
        (let ((message (current-message)))
          (slime-check "Minibuffer contains: \"3\""
            (equal "=> 3 (#x3, #o3, #b11)" message)))))))

(def-slime-test interrupt-bubbling-idiot 
    ()
    "Test interrupting a loop that sends a lot of output to Emacs."
    '(())
  (slime-accept-process-output nil 1)
  (slime-check-top-level)
  (slime-eval-async '(cl:loop :for i :from 0 :do (cl:progn (cl:print i) 
                                                           (cl:finish-output)))
                    (lambda (_) ) 
                    "CL-USER")
  (sleep-for 1)
  (slime-interrupt)
  (slime-wait-condition "Debugger visible" 
                        (lambda () 
                          (and (slime-sldb-level= 1)
                               (get-buffer-window (sldb-get-default-buffer))))
                        30)
  (with-current-buffer (sldb-get-default-buffer)
    (sldb-quit))
  (slime-sync-to-top-level 5))

(def-slime-test package-updating
    (package-name nicknames)
    "Test if slime-lisp-package is updated."
    '(("COMMON-LISP" ("CL"))
      ("KEYWORD" ("" "KEYWORD"))
      ("COMMON-LISP-USER" ("CL-USER")))
  (with-current-buffer (slime-output-buffer)
    (let ((p (slime-eval 
              `(swank:listener-eval 
                ,(format 
                  "(cl:setq cl:*print-case* :upcase)
                   (cl:setq cl:*package* (cl:find-package %S))
                   (cl:package-name cl:*package*)" package-name))
              (slime-lisp-package))))
      (slime-check ("slime-lisp-package is %S." package-name)
        (equal (slime-lisp-package) package-name))
      (slime-check ("slime-lisp-package-prompt-string is in %S." nicknames)
        (member (slime-lisp-package-prompt-string) nicknames)))))

(def-slime-test repl-test
    (input result-contents)
    "Test simple commands in the minibuffer."
    '(("(+ 1 2)" "SWANK> (+ 1 2)
{}3
SWANK> *[]")
      ("(princ 10)" "SWANK> (princ 10)
{10}10
SWANK> *[]")
      ("(princ 10)(princ 20)" "SWANK> (princ 10)(princ 20)
{1020}20
SWANK> *[]")
      ("(dotimes (i 10 77) (princ i) (terpri))" 
       "SWANK> (dotimes (i 10 77) (princ i) (terpri))
{0
1
2
3
4
5
6
7
8
9
}77
SWANK> *[]")
      ("(abort)" "SWANK> (abort)
{}; Evaluation aborted.
SWANK> *[]")
      ("(progn (princ 10) (force-output) (abort))" 
       "SWANK> (progn (princ 10) (force-output) (abort))
{10}; Evaluation aborted.
SWANK> *[]")
      ("(progn (princ 10) (abort))" 
       ;; output can be flushed after aborting
       "SWANK> (progn (princ 10) (abort))
{10}; Evaluation aborted.
SWANK> *[]")
      ("(values 1 2 3)" "SWANK> (values 1 2 3)
{}1
2
3
SWANK> *[]"))
  (with-current-buffer (slime-output-buffer)
    (setf (slime-lisp-package-prompt-string) "SWANK"))
  (kill-buffer (slime-output-buffer))
  (with-current-buffer (slime-output-buffer)
    (insert input)
    (slime-check-buffer-contents "Buffer contains input" 
                                 (concat "{}SWANK> [" input "*]"))
    (call-interactively 'slime-repl-return)
    (slime-sync-to-top-level 5)
    (slime-check-buffer-contents "Buffer contains result" result-contents)))

(defun slime-check-buffer-contents (msg expected)
  (let* ((marks '((point . ?*) 
                  (slime-output-start . ?{) (slime-output-end . ?}) 
                  (slimerepl-input-start-mark . ?\[) (point-max . ?\])))
         (marks (remove-if-not (lambda (m) (position (cdr m) expected))
                               marks))
         (marks (sort (copy-sequence marks) 
                      (lambda (x y)
                        (< (position (cdr x) expected)
                           (position (cdr y) expected)))))
         (content (remove-if (lambda (c) (member* c marks :key #'cdr))
                             expected))
         (marks (do ((result '() (acons (caar m) (1+ (position (cdar m) s))
                                        result))
                     (m marks (cdr m))
                     (s expected (remove* (cdar m) s)))
                    ((null m) (reverse result))))
         (point (point))
         (point-max (point-max)))
    (slime-test-expect (concat msg " [content]") content (buffer-string))
    (macrolet ((test-mark 
                (mark)
                `(when (assoc ',mark marks)
                   (slime-test-expect (format "%s [%s]" msg ',mark)
                                      (cdr (assoc ',mark marks))
                                      ,mark
                                      #'=))))
      (test-mark point)
      (test-mark slime-output-end)
      (test-mark slime-output-start)
      (test-mark slime-repl-input-start-mark)
      (test-mark point-max))))

(def-slime-test repl-return 
    (before after result-contents)
    "Test if slime-repl-return sends the correct protion to Lisp even
if point is not at the end of the line."
    '(("(+ 1 2)" "" "SWANK> (+ 1 2)
3
SWANK> ")
("(+ 1 " "2)" "SWANK> (+ 1 2)
3
SWANK> ")

("(+ 1\n" "2)" "SWANK> (+ 1
2)
3
SWANK> "))
  (with-current-buffer (slime-output-buffer)
    (setf (slime-lisp-package-prompt-string) "SWANK"))
  (kill-buffer (slime-output-buffer))
  (with-current-buffer (slime-output-buffer)
    (insert before)
    (save-excursion (insert after))
    (slime-test-expect "Buffer contains input" 
                       (concat "SWANK> " before after)
                       (buffer-string))
    (call-interactively 'slime-repl-return)
    (slime-sync-to-top-level 5)
    (slime-test-expect "Buffer contains result" 
                       result-contents (buffer-string))))
  
(def-slime-test repl-read
    (prompt input result-contents)
    "Test simple commands in the minibuffer."
    '(("(read-line)" "foo" "SWANK> (values (read-line))
foo
\"foo\"
SWANK> ")
      ("(read-char)" "1" "SWANK> (values (read-char))
1
#\\1
SWANK> ")
      ("(read)" "(+ 2 3
4)" "SWANK> (values (read))
\(+ 2 3
4)
\(+ 2 3 4)
SWANK> "))
  (with-current-buffer (slime-output-buffer)
    (setf (slime-lisp-package-prompt-string) "SWANK"))
  (kill-buffer (slime-output-buffer))
  (with-current-buffer (slime-output-buffer)
    (insert (format "(values %s)" prompt))
    (call-interactively 'slime-repl-return)
    (slime-wait-condition "reading" #'slime-reading-p 5)
    (insert input)
    (call-interactively 'slime-repl-return)
    (slime-sync-to-top-level 5)
    (slime-test-expect "Buffer contains result" 
                       result-contents (buffer-string))))

(def-slime-test repl-read-lines
    (command inputs final-contents)
    "Test reading multiple lines from the repl."
    '(("(list (read-line) (read-line) (read-line))" 
       ("a" "b" "c")
       "SWANK> (list (read-line) (read-line) (read-line))
a
b
c
\(\"a\" \"b\" \"c\")
SWANK> "))
  (when (slime-output-buffer)
    (kill-buffer (slime-output-buffer)))
  (with-current-buffer (slime-output-buffer)
    (setf (slime-lisp-package-prompt-string) "SWANK")
    (insert command)
    (call-interactively 'slime-repl-return)
    (dolist (input inputs) 
      (slime-wait-condition "reading" #'slime-reading-p 5)
      (insert input)
      (call-interactively 'slime-repl-return))
    (slime-sync-to-top-level 5)
    (slime-check "Buffer contains result"
      (equal final-contents (buffer-string)))))

(def-slime-test repl-type-ahead
    (command input final-contents)
    "Ensure that user input is preserved correctly.
In particular, input inserted while waiting for a result."
    '(("(sleep 0.1)" "foo*" "SWANK> (sleep 0.1)
{}NIL
SWANK> [foo*]")
      ("(sleep 0.1)" "*foo" "SWANK> (sleep 0.1)
{}NIL
SWANK> [*foo]")
      ("(progn (sleep 0.1) (abort))" "*foo" "SWANK> (progn (sleep 0.1) (abort))
{}; Evaluation aborted.
SWANK> [*foo]"))
  (when (slime-output-buffer)
    (kill-buffer (slime-output-buffer)))
  (setf (slime-lisp-package-prompt-string) "SWANK")
  (with-current-buffer (slime-output-buffer)
    (insert command)
    (call-interactively 'slime-repl-return)
    (save-excursion (insert (delete* ?* input)))
    (forward-char (position ?* input))
    (slime-sync-to-top-level 5)
    (slime-check-buffer-contents "Buffer contains result" final-contents)))

(def-slime-test interactive-eval-output
    (input result-contents visiblep &optional later)
    "Test simple commands in the minibuffer."
    `(("(+ 1 2)" "SWANK> 
;;;; (+ 1 2) ...
{}SWANK> *[]" nil)
      ("(princ 10)" "SWANK> 
;;;; (princ 10) ...
{10}
SWANK> *[]" t)
      ("(princ 11)" "SWANK> 
;;;; (princ 11) ...
{1122}
SWANK> *[]" t "22")
;;       ,@(when (eq window-system 'x)
;;           '(("(princ \"����������������������������\")"
;;              "SWANK> 
;; ;;;; (princ \"����������������������������\") ...
;; ����������������������������
;; SWANK> *" t)))
      ("(abort)" "SWANK> 
;;;; (abort) ...
{}SWANK> *[]" nil)
      ("(progn (princ 10) (finish-output) (abort))" "SWANK> 
;;;; (progn (princ 10) (finish-output) (abort)) ...
{10}
SWANK> *[]" t)
      ("(progn (princ 11) (finish-output) (abort))" "SWANK> 
;;;; (progn (princ 11) (finish-output) (abort)) ...
{1122}
SWANK> *[]" t "22")
      ("(+ 3 4)" "SWANK> 
;;;; (+ 3 4) ...
{22}
SWANK> *[]" nil "22"))
  (with-current-buffer (slime-output-buffer)
    (setf (slime-lisp-package-prompt-string) "SWANK"))
  (kill-buffer (slime-output-buffer))
  (with-current-buffer (slime-output-buffer)
    (slime-interactive-eval input) 
    (slime-sync-to-top-level 2)
    (when later
      (setq slime-repl-popup-on-output nil)
      (slime-eval-async `(cl:write-string ,later))
      (slime-sync-to-top-level 2))
    (slime-check-buffer-contents "Buffer contains result" result-contents)
    (unless noninteractive
      (sit-for 0.1)
      (let ((window (get-buffer-window (current-buffer))))
        (slime-test-expect "Buffer visible?" visiblep (not (not window)))
        (slime-test-expect "EOB visible?" visiblep
                           (and window
                                (pos-visible-in-window-p (point-max) 
                                                         window)))))))


(def-slime-test inspector
    (exp)
    "Test basic inspector workingness."
    '(((let ((h (make-hash-table)))
         (loop for i below 10 do (setf (gethash i h) i))
         h))
      ((make-array 10))
      ((make-list 10))
      ('cons)
      (#'cons))
  (slime-inspect (prin1-to-string exp))
  (assert (not (slime-inspector-visible-p)))
  (slime-wait-condition "Inspector visible" #'slime-inspector-visible-p 5)
  (with-current-buffer (window-buffer (selected-window))
    (slime-inspector-quit))
  (slime-wait-condition "Inspector closed" 
                        (lambda () (not (slime-inspector-visible-p)))
                        5)
  (slime-sync-to-top-level 1))

(defun slime-inspector-visible-p ()
  (let ((buffer (window-buffer (selected-window))))
    (string-match "\\*Slime Inspector\\*" 
                  (buffer-name buffer))))

(def-slime-test break
    (times exp)
    "Test whether BREAK invokes SLDB."
    (let ((exp1 '(break)))
      `((1 ,exp1) (2 ,exp1) (3 ,exp1)))
  (slime-accept-process-output nil 0.2)
  (slime-check-top-level)
  (slime-eval-async 
   `(cl:eval (cl:read-from-string 
              ,(prin1-to-string `(dotimes (i ,times) ,exp (sleep 0.2))))))
  (dotimes (i times)
    (slime-wait-condition "Debugger visible" 
                          (lambda () 
                            (and (slime-sldb-level= 1)
                                 (get-buffer-window 
                                  (sldb-get-default-buffer))))
                          1)
    (with-current-buffer (sldb-get-default-buffer)
      (sldb-continue))
    (slime-wait-condition "sldb closed" 
                          (lambda () (not (sldb-get-default-buffer)))
                          0.2))
  (slime-sync-to-top-level 1))

(def-slime-test (break2 ("cmucl" "allegro" "ccl"))
    (times exp)
    "Backends should arguably make sure that BREAK does not depend
on *DEBUGGER-HOOK*."
    (let ((exp2 
           '(block outta
              (let ((*debugger-hook* (lambda (c h) (return-from outta 42))))
                (break)))))
      `((1 ,exp2) (2 ,exp2) (3 ,exp2)))
  (slime-test-break times exp))

(def-slime-test locally-bound-debugger-hook
    ()
    "Test that binding *DEBUGGER-HOOK* locally works properly."
    '(())
  (slime-accept-process-output nil 1)
  (slime-check-top-level)
  (slime-compile-string
   (prin1-to-string `(defun cl-user::quux ()
                       (block outta
                         (let ((*debugger-hook*
                                #'(lambda (c hook)
                                    (declare (ignore c hook))
                                    (return-from outta 42))))
                           (error "FOO")))))
   0)
  (slime-sync-to-top-level 2)
  (slime-eval-async '(cl-user::quux))
  ;; FIXME: slime-wait-condition returns immediately if the test returns true
  (slime-wait-condition "Checking that Debugger does not popup" 
                        (lambda () 
                          (not (sldb-get-default-buffer)))
                        3)
  (slime-sync-to-top-level 5))


(def-slime-test interrupt-at-toplevel
    ()
    "Let's see what happens if we send a user interrupt at toplevel."
    '(())
  (slime-check-top-level)
  (slime-interrupt)
  (slime-wait-condition "Debugger visible" 
                        (lambda () 
                          (and (slime-sldb-level= 1)
                               (get-buffer-window (sldb-get-default-buffer))))
                        5)
  (with-current-buffer (sldb-get-default-buffer)
    (sldb-invoke-restart (slime-test-find-top-level-restart)))
  (slime-sync-to-top-level 5))

(defun slime-test-find-top-level-restart ()
  (let ((case-fold-search t))
    (or (loop for i from 0  for (name str) in sldb-restarts 
              when (string-match "SLIME's top level" str) return i)
        (loop for i from 0  for (name str) in sldb-restarts 
              when (and (string-match "abort" name) (string-match "top" str))
              return i))))

(def-slime-test interrupt-in-blocking-read
    ()
    "Let's see what happens if we interrupt a blocking read operation."
    '(())
  (slime-check-top-level)
  (when (slime-output-buffer)
    (setf (slime-lisp-package-prompt-string) "SWANK")
    (kill-buffer (slime-output-buffer)))
  (with-current-buffer (slime-output-buffer)
    (insert "(read-char)")
    (call-interactively 'slime-repl-return))
  (slime-wait-condition "reading" #'slime-reading-p 5)
  (slime-interrupt)
  (slime-wait-condition "Debugger visible" 
                        (lambda () 
                          (and (slime-sldb-level= 1)
                               (get-buffer-window (sldb-get-default-buffer))))
                        5)
  (with-current-buffer (sldb-get-default-buffer)
    (sldb-continue))
  (slime-wait-condition "reading" #'slime-reading-p 5)
  (with-current-buffer (slime-output-buffer)
    (insert "X")
    (call-interactively 'slime-repl-return)
    (slime-sync-to-top-level 5)
    (slime-test-expect "Buffer contains result" 
                       "SWANK> (read-char)
X
#\\X
SWANK> " (buffer-string))))

(def-slime-test interrupt-in-debugger (interrupts continues)
    "Let's see what happens if we interrupt the debugger.
INTERRUPTS ... number of nested interrupts
CONTINUES  ... how often the continue restart should be invoked"
    '((1 0) (2 1) (4 2))
  (slime-check "No debugger" (not (sldb-get-default-buffer)))
  (dotimes (i interrupts)
    (slime-interrupt)
    (let ((level (1+ i)))
      (slime-wait-condition (format "Debug level %d reachend" level)
                            (lambda () (equal (sldb-level) level))
                            2)))
  (dotimes (i continues)
    (with-current-buffer (sldb-get-default-buffer)
      (sldb-continue))
    (let ((level (- interrupts (1+ i))))
      (slime-wait-condition (format "Return to debug level %d" level)
                            (lambda () (equal (sldb-level) level))
                            2)))
  (with-current-buffer (sldb-get-default-buffer)
    (sldb-invoke-restart (slime-test-find-top-level-restart)))
  (slime-sync-to-top-level 1))
    
(def-slime-test disconnect
    ()
    "Close the connetion.
Confirm that the subprocess continues gracefully.
Reconnect afterwards."
    '(())
  (slime-check-top-level)
  (let* ((c (slime-connection))
         (p (slime-inferior-process c)))
    (with-current-buffer (process-buffer p)
      (erase-buffer))
    (delete-process c)
    (assert (equal (process-status c) 'closed) nil "Connection not closed")
    (slime-accept-process-output nil 0.1)
    (assert (equal (process-status p) 'run) nil "Subprocess not running")
    (with-current-buffer (process-buffer p)
      (assert (< (buffer-size) 500) nil "Unusual output"))
    (slime-inferior-connect p (slime-inferior-lisp-args p))
    (lexical-let ((hook nil) (p p))
      (setq hook (lambda ()
                   (slime-test-expect 
                    "We are connected again" p (slime-inferior-process))
                   (remove-hook 'slime-connected-hook hook)))
      (add-hook 'slime-connected-hook hook)
      (slime-wait-condition "Lisp restarted" 
                            (lambda () 
                              (not (member hook slime-connected-hook)))
                            5))))
    

;;;; Utilities

;;;;; Misc.

(defun slime-length= (seq n)
  "Test for whether SEQ contains N number of elements. I.e. it's equivalent
 to (= (LENGTH SEQ) N), but besides being more concise, it may also be more
 efficiently implemented."
  (etypecase seq
    (list
     (let ((list seq))
       (setq list (nthcdr (1- n) list))
       (and list (null (cdr list)))))
    (sequence
     (= (length seq) n))))

(defun slime-length> (seq n)
  "Return non-nil if (> (length LIST) N)."
  (etypecase seq
    (list (nthcdr n seq))
    (sequence (> (length seq) n))))

(defun slime-trim-whitespace (str)
  (save-match-data
    (string-match "^\\s-*\\(.*?\\)\\s-*$" str)
    (match-string 1 str)))

;;;;; Buffer related

(defun slime-buffer-narrowed-p (&optional buffer)
  "Returns T if BUFFER (or the current buffer respectively) is narrowed."
  (with-current-buffer (or buffer (current-buffer))
    (let ((beg (point-min))
          (end (point-max))
          (total (buffer-size)))
      (or (/= beg 1) (/= end (1+ total))))))

(defun slime-column-max ()
  (save-excursion
    (goto-char (point-min))
    (loop for column = (prog2 (end-of-line) (current-column) (forward-line))
          until (= (point) (point-max))
          maximizing column)))

;;;;; CL symbols vs. Elisp symbols.

(defun slime-cl-symbol-name (symbol)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match ":\\([^:]*\\)$" n)
	(let ((symbol-part (match-string 1 n)))
          (if (string-match "^|\\(.*\\)|$" symbol-part)
              (match-string 1 symbol-part)
              symbol-part))
      n)))

(defun slime-cl-symbol-package (symbol &optional default)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match "^\\([^:]*\\):" n)
	(match-string 1 n)
      default)))

(defun slime-qualify-cl-symbol-name (symbol-or-name)
  "Return a package-qualified symbol-name that indicates the CL symbol
SYMBOL. If SYMBOL doesn't already have a package prefix the current
package is used."
  (let ((s (if (stringp symbol-or-name)
               symbol-or-name
             (symbol-name symbol-or-name))))
    (if (slime-cl-symbol-package s)
        s
      (format "%s::%s"
              (let* ((package (or (slime-current-package) (slime-lisp-package))))
                ;; package is a string like ":cl-user" or "CL-USER".
                (if (and package (string-match "^:" package))
                    (substring package 1)
                  package))
              (slime-cl-symbol-name s)))))

;;;;; Moving, CL idiosyncracies aware (reader conditionals &c.)

(defmacro slime-point-moves-p (&rest body)
  "Execute BODY and return true if the current buffer's point moved."
  (let ((pointvar (gensym "point-")))
    `(let ((,pointvar (point)))
       (save-current-buffer ,@body)
       (/= ,pointvar (point)))))

(put 'slime-point-moves-p 'lisp-indent-function 0)

(defun slime-forward-sexp (&optional count)
  "Like `forward-sexp', but understands reader-conditionals (#- and #+)."
  (dotimes (i (or count 1))
    (while (slime-point-moves-p (slime-forward-blanks)
                                (slime-forward-reader-comment)
                                (slime-forward-reader-conditional)))
    (forward-sexp)))

(defun slime-forward-blanks ()
  "Move forward over all whitespace and newlines at point."
  (ignore-errors
    (while (slime-point-moves-p
             (skip-syntax-forward " ")
             ;; newlines aren't in lisp-mode's whitespace syntax class
             (when (eolp) (forward-char))))))

;; Emacs 21's forward-sexp understands #| |# comments in lisp-mode
;; buffers, but (at least) Emacs 20's doesn't, so here it is.
(defun slime-forward-reader-comment ()
  "Move forward over #|...|# reader comments. The comments may be nested."
  (when (looking-at "#|")
    (goto-char (match-end 0))
    (while (not (looking-at "|#"))
      (re-search-forward (regexp-opt '("|#" "#|")))
      (goto-char (match-beginning 0))
      (when (looking-at "#|")           ; nested comment
        (slime-forward-reader-comment)))
    (goto-char (match-end 0))))

(defun slime-forward-reader-conditional ()
  "Move past any reader conditional (#+ or #-) at point."
  (when (or (looking-at "#\\+")
            (looking-at "#-"))
    (goto-char (match-end 0))
    (let* ((plus-conditional-p (eq (char-before) ?+))
           (result (slime-eval-feature-conditional (read (current-buffer)))))
      (unless (if plus-conditional-p result (not result))
        ;; skip this sexp
        (slime-forward-sexp)))))

(defun slime-keywordify (symbol)
  "Make a keyword out of the symbol SYMBOL."
  (let ((name (downcase (symbol-name symbol))))
    (intern (if (eq ?: (aref name 0)) 
                name 
              (concat ":" name)))))

(defun slime-eval-feature-conditional (e)
  "Interpret a reader conditional expression."
  (if (symbolp e)
      (memq (slime-keywordify e) (slime-lisp-features))
    (funcall (ecase (slime-keywordify (car e))
               (:and #'every)
               (:or #'some)
               (:not (lambda (f l) (not (apply f l)))))
             #'slime-eval-feature-conditional
             (cdr e))))

;;;;; Extracting Lisp forms from the buffer or user

(defun slime-defun-at-point ()
  "Return the text of the defun at point."
  (apply #'buffer-substring-no-properties
         (slime-region-for-defun-at-point)))

(defun slime-region-for-defun-at-point ()
  "Return the start and end position of the toplevel form at point."
  (save-excursion
    (save-match-data
      (end-of-defun)
      (let ((end (point)))
        (beginning-of-defun)
        (list (point) end)))))

(defun slime-beginning-of-symbol ()
  "Move point to the beginning of the current symbol."
  (when (slime-point-moves-p
          (while (slime-point-moves-p 
                   (skip-syntax-backward "w_")
                   (when (eq (char-before) ?|)
                     (backward-char)))))
    (when (eq (char-before) ?#) ; special case for things like "#<foo"
      (forward-char))))

(defun slime-end-of-symbol ()
  "Move point to the end of the current symbol."
  (while (slime-point-moves-p 
           (skip-syntax-forward "w_")
           ;; | has the syntax as ", so we need to 
           ;; treat it manually rather than via syntax. 
           (when (looking-at "|")
             (forward-char)))))

(put 'slime-symbol 'end-op 'slime-end-of-symbol)
(put 'slime-symbol 'beginning-op 'slime-beginning-of-symbol)

(defun slime-symbol-start-pos ()
  "Return the starting position of the symbol under point.
The result is unspecified if there isn't a symbol under the point."
  (save-excursion (slime-beginning-of-symbol) (point)))

(defun slime-symbol-end-pos ()
  (save-excursion (slime-end-of-symbol) (point)))

;; FIXME: rename this as slime-symbol-at-point.
(defun slime-symbol-name-at-point ()
  "Return the name of the symbol at point, otherwise nil."
  (save-restriction
    ;; Don't be tricked into grabbing the REPL prompt.
    (when (and (eq major-mode 'slime-repl-mode)
               (>= (point) slime-repl-input-start-mark))
      (narrow-to-region slime-repl-input-start-mark (point-max)))
    (save-excursion
      (let ((string (thing-at-point 'slime-symbol)))
        (and string
             ;; In Emacs20 (thing-at-point 'symbol) returns "" instead
             ;; of nil when called from an empty (or
             ;; narrowed-to-empty) buffer.
             (not (equal string ""))
             (substring-no-properties string))))))

(defun slime-sexp-at-point ()
  "Return the sexp at point as a string, otherwise nil."
  (let ((string (thing-at-point 'sexp)))
    (if string (substring-no-properties string) nil)))

(defun slime-sexp-at-point-or-error ()
  "Return the sexp at point as a string, othwise signal an error."
  (or (slime-sexp-at-point)
      (error "No expression at point.")))


;;;; Portability library

(when (featurep 'xemacs)
  (require 'overlay))

(defun slime-split-string (string &optional separators omit-nulls)
  "This is like `split-string' in Emacs22, but also works in
Emacs20 and 21."
  (let ((splits (split-string string separators)))
    (if omit-nulls
        (setq splits (remove "" splits))
      ;; SPLIT-STRING in Emacs before 22.x automatically removed nulls
      ;; at beginning and end, so we gotta add them here again.
      (when (or (slime-emacs-20-p) (slime-emacs-21-p))
        (when (find (elt string 0) separators)
          (push "" splits))
        (when (find (elt string (1- (length string))) separators)
          (setq splits (append splits (list ""))))))
    splits))

(defun slime-delete-and-extract-region (start end)
  "Like `delete-and-extract-region' except that it is guaranteed
to return a string. At least Emacs 21.3.50 returned `nil' on
\(delete-and-extract-region (point) (point)), this function
will return \"\"."
  (let ((result (delete-and-extract-region start end)))
    (if (null result)
        ""
      (assert (stringp result))
      result)))

(defmacro slime-defun-if-undefined (name &rest rest)
  ;; We can't decide at compile time whether NAME is properly
  ;; bound. So we delay the decision to runtime to ensure some
  ;; definition
  `(unless (fboundp ',name)
     (defun ,name ,@rest)))

(put 'slime-defun-if-undefined 'lisp-indent-function 2)

(defvar slime-accept-process-output-supports-floats 
  (ignore-errors (accept-process-output nil 0.0) t))

(defun slime-accept-process-output (&optional process timeout)
  "Like `accept-process-output' but the TIMEOUT argument can be a float."
  (cond (slime-accept-process-output-supports-floats
         (accept-process-output process timeout))
        (t
         (accept-process-output process 
                                (if timeout (truncate timeout))
                                ;; Emacs 21 uses microsecs; Emacs 22 millisecs
                                (if timeout (truncate (* timeout 1000000)))))))

(defun slime-pop-to-buffer (buffer &optional other-window)
  "Select buffer BUFFER in some window.
This is like `pop-to-buffer' but also sets the input focus
for (somewhat) better multiframe support."
  (set-buffer buffer)
  (let ((old-frame (selected-frame))
        (window (display-buffer buffer other-window)))
    (select-window window)
    ;; select-window doesn't set the input focus
    (when (and (not (featurep 'xemacs))
               (>= emacs-major-version 22)
               (not (eq old-frame (selected-frame))))
      (select-frame-set-input-focus (window-frame window))))
  buffer)

(defun slime-add-local-hook (hook function &optional append)
  (cond ((featurep 'xemacs) (add-local-hook hook function append))
        ((< emacs-major-version 21)
         (make-local-hook hook)
         (add-hook hook function append t))
        (t (add-hook hook function append t))))

(slime-defun-if-undefined next-single-char-property-change
    (position prop &optional object limit)
  (let ((limit (typecase limit
		 (null nil)
		 (marker (marker-position limit))
		 (t limit))))
    (if (stringp object)
	(or (next-single-property-change position prop object limit)
	    limit 
	    (length object))
      (with-current-buffer (or object (current-buffer))
	(let ((initial-value (get-char-property position prop object))
	      (limit (or limit (point-max))))
	  (loop for pos = position then (next-char-property-change pos limit)
		if (>= pos limit) return limit
		if (not (eq initial-value 
			    (get-char-property pos prop object))) 
		return pos))))))

(slime-defun-if-undefined previous-single-char-property-change 
    (position prop &optional object limit)
  (let ((limit (typecase limit
		 (null nil)
		 (marker (marker-position limit))
		 (t limit))))
    (if (stringp object)
	(or (previous-single-property-change position prop object limit)
	    limit 
	    (length object))
      (with-current-buffer (or object (current-buffer))
	(let ((limit (or limit (point-min))))
	  (if (<= position limit)
	      limit
            (let ((initial-value (get-char-property (1- position)
                                                    prop object)))
              (loop for pos = position then 
                    (previous-char-property-change pos limit)
                    if (<= pos limit) return limit
                    if (not (eq initial-value 
                                (get-char-property (1- pos) prop object))) 
                    return pos))))))))

(slime-defun-if-undefined next-char-property-change (position &optional limit)
  (let ((tmp (next-overlay-change position)))
    (when tmp
      (setq tmp (min tmp limit)))
    (next-property-change position nil tmp)))

(slime-defun-if-undefined previous-char-property-change 
    (position &optional limit)
  (let ((tmp (previous-overlay-change position)))
    (when tmp
      (setq tmp (max tmp limit)))
    (previous-property-change position nil tmp)))
        
(slime-defun-if-undefined substring-no-properties (string &optional start end)
  (let* ((start (or start 0))
	 (end (or end (length string)))
	 (string (substring string start end)))
    (set-text-properties 0 (- end start) nil string)
    string))

(slime-defun-if-undefined match-string-no-properties (num &optional string)
  (if (match-beginning num)
      (if string
	  (substring-no-properties string (match-beginning num)
				   (match-end num))
	(buffer-substring-no-properties (match-beginning num)
                                        (match-end num)))))

(slime-defun-if-undefined set-window-text-height (window height)
  (let ((delta (- height (window-text-height window))))
    (unless (zerop delta)
      (let ((window-min-height 1))
	(if (and window (not (eq window (selected-window))))
	    (save-selected-window
	      (select-window window)
	      (enlarge-window delta))
	  (enlarge-window delta))))))

(slime-defun-if-undefined window-text-height (&optional window)
  (1- (window-height window)))

(slime-defun-if-undefined subst-char-in-string (fromchar tochar string 
						   &optional inplace)
  "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
  (let ((i (length string))
	(newstr (if inplace string (copy-sequence string))))
    (while (> i 0)
      (setq i (1- i))
      (if (eq (aref newstr i) fromchar)
	  (aset newstr i tochar)))
    newstr))

(slime-defun-if-undefined count-screen-lines 
  (&optional beg end count-final-newline window)
  (unless beg
    (setq beg (point-min)))
  (unless end
    (setq end (point-max)))
  (if (= beg end)
      0
    (save-excursion
      (save-restriction
        (widen)
        (narrow-to-region (min beg end)
                          (if (and (not count-final-newline)
                                   (= ?\n (char-before (max beg end))))
                              (1- (max beg end))
                            (max beg end)))
        (goto-char (point-min))
        ;; XXX make this xemacs compatible
        (1+ (vertical-motion (buffer-size) window))))))

(slime-defun-if-undefined seconds-to-time (seconds)
  "Convert SECONDS (a floating point number) to a time value."
  (list (floor seconds 65536)
	(floor (mod seconds 65536))
	(floor (* (- seconds (ffloor seconds)) 1000000))))

(slime-defun-if-undefined time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(slime-defun-if-undefined time-add (t1 t2)
  "Add two time values.  One should represent a time difference."
  (let ((high (car t1))
	(low (if (consp (cdr t1)) (nth 1 t1) (cdr t1)))
	(micro (if (numberp (car-safe (cdr-safe (cdr t1))))
		   (nth 2 t1)
		 0))
	(high2 (car t2))
	(low2 (if (consp (cdr t2)) (nth 1 t2) (cdr t2)))
	(micro2 (if (numberp (car-safe (cdr-safe (cdr t2))))
		    (nth 2 t2)
		  0)))
    ;; Add
    (setq micro (+ micro micro2))
    (setq low (+ low low2))
    (setq high (+ high high2))

    ;; Normalize
    ;; `/' rounds towards zero while `mod' returns a positive number,
    ;; so we can't rely on (= a (+ (* 100 (/ a 100)) (mod a 100))).
    (setq low (+ low (/ micro 1000000) (if (< micro 0) -1 0)))
    (setq micro (mod micro 1000000))
    (setq high (+ high (/ low 65536) (if (< low 0) -1 0)))
    (setq low (logand low 65535))

    (list high low micro)))

(slime-defun-if-undefined line-beginning-position (&optional n)
  (save-excursion
    (beginning-of-line n)
    (point)))

(slime-defun-if-undefined line-end-position (&optional n)
  (save-excursion
    (end-of-line n)
    (point)))

(slime-defun-if-undefined check-parens ()
    "Verify that parentheses in the current buffer are balanced.
If they are not, position point at the first syntax error found."
    (interactive)
    (let ((saved-point (point))
	  (state (parse-partial-sexp (point-min) (point-max) -1)))
      (destructuring-bind (depth innermost-start last-terminated-start
				 in-string in-comment after-quote 
				 minimum-depth comment-style 
				 comment-or-string-start &rest _) state
	(cond ((and (zerop depth) 
		    (not in-string) 
		    (or (not in-comment) 
			(and (eq comment-style nil) 
			     (eobp)))
		    (not after-quote))
	       (goto-char saved-point)
	       (message "All parentheses appear to be balanced."))
	      ((plusp depth)
	       (goto-char innermost-start)
	       (error "Missing )"))
	      ((minusp depth)
	       (error "Extra )"))
	      (in-string
	       (goto-char comment-or-string-start)
	       (error "String not terminated"))
	      (in-comment
	       (goto-char comment-or-string-start)
	       (error "Comment not terminated"))
	      (after-quote
	       (error "After quote"))
	      (t (error "Shouldn't happen: parsing state: %S" state))))))

(slime-defun-if-undefined read-directory-name (prompt 
                                               &optional dir default-dirname
                                               mustmatch initial)
  (unless dir
    (setq dir default-directory))
  (unless default-dirname
    (setq default-dirname
	  (if initial (concat dir initial) default-directory)))
  (let ((file (read-file-name prompt dir default-dirname mustmatch initial)))
    (setq file (file-name-as-directory (expand-file-name file)))
    (cond ((file-directory-p file)
           file)
          (t 
           (error "Not a directory: %s" file)))))

(slime-defun-if-undefined check-coding-system (coding-system)
  (or (eq coding-system 'binary)
      (error "No such coding system: %S" coding-system)))

(slime-defun-if-undefined process-coding-system (process)
  '(binary . binary))

(slime-defun-if-undefined set-process-coding-system 
    (process &optional decoding encoding))

(unless (boundp 'temporary-file-directory)
  (defvar temporary-file-directory
    (file-name-as-directory
     (cond ((memq system-type '(ms-dos windows-nt))
            (or (getenv "TEMP") (getenv "TMPDIR") (getenv "TMP") "c:/temp"))
           ((memq system-type '(vax-vms axp-vms))
            (or (getenv "TMPDIR") (getenv "TMP") 
                (getenv "TEMP") "SYS$SCRATCH:"))
           (t
            (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "/tmp"))))
    "The directory for writing temporary files."))

(unless (fboundp 'with-temp-message)
  (defmacro with-temp-message (message &rest body)
    (let ((current-message (make-symbol "current-message"))
          (temp-message (make-symbol "with-temp-message")))
      `(let ((,temp-message ,message)
             (,current-message))
         (unwind-protect
             (progn
               (when ,temp-message
                 (setq ,current-message (current-message))
                 (message "%s" ,temp-message))
               ,@body)
           (and ,temp-message ,current-message
                (message "%s" ,current-message)))))))

(defun slime-emacs-20-p ()
  (and (not (featurep 'xemacs))
       (= emacs-major-version 20)))

(defun slime-emacs-21-p ()
  (and (not (featurep 'xemacs))
       (= emacs-major-version 21)))

(when (featurep 'xemacs)
  (add-hook 'sldb-hook 'sldb-xemacs-emulate-point-entered-hook))

(defun sldb-xemacs-emulate-point-entered-hook ()
  (add-hook (make-local-variable 'post-command-hook)
            'sldb-xemacs-post-command-hook))

(defun sldb-xemacs-post-command-hook ()
  (when (get-text-property (point) 'point-entered)
    (funcall (get-text-property (point) 'point-entered))))

(slime-defun-if-undefined with-selected-window (window &rest body)
  `(save-selected-window
     (select-window ,window)
     ,@body))


;;;; Finishing up

(require 'bytecomp)
(let ((byte-compile-warnings '()))
  (mapc #'byte-compile
        '(slime-alistify
          slime-log-event
          slime-events-buffer
          slime-write-string 
          slime-repl-emit
          slime-output-buffer
          slime-connection-output-buffer
          slime-output-filter
          slime-repl-show-maximum-output
          slime-process-available-input 
          slime-dispatch-event 
          slime-net-filter 
          slime-net-have-input-p
          slime-net-decode-length
          slime-net-read
          slime-print-apropos
          slime-show-note-counts
          slime-insert-propertized
          slime-tree-insert)))

(provide 'slime)
(run-hooks 'slime-load-hook)

;; Local Variables: 
;; outline-regexp: ";;;;+"
;; indent-tabs-mode: nil
;; coding: latin-1-unix
;; compile-command: "emacs -batch -L . -eval '(byte-compile-file \"slime.el\")' ; rm -v slime.elc"
;; End:
;;; slime.el ends here
