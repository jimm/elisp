;;; -*- outline-regexp:";;;;;*" indent-tabs-mode:nil coding:latin-1-unix -*-
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;;; swank.lisp
;;;
;;; This file defines the "Swank" TCP server for Emacs to talk to. The
;;; code in this file is purely portable Common Lisp. We do require a
;;; smattering of non-portable functions in order to write the server,
;;; so we have defined them in `swank-backend.lisp' and implemented
;;; them separately for each Lisp implementation. These extensions are
;;; available to us here via the `SWANK-BACKEND' package.

(defpackage :swank
  (:use :cl :swank-backend)
  (:export #:startup-multiprocessing
           #:start-server 
           #:create-server
           #:stop-server
           #:restart-server
           #:ed-in-emacs
           #:inspect-in-emacs
           #:print-indentation-lossage
           #:invoke-slime-debugger
           #:swank-debugger-hook
           #:emacs-inspect
           ;;#:inspect-slot-for-emacs
           ;; These are user-configurable variables:
           #:*communication-style*
           #:*dont-close*
           #:*log-events*
           #:*log-output*
           #:*use-dedicated-output-stream*
           #:*dedicated-output-stream-port*
           #:*configure-emacs-indentation*
           #:*readtable-alist*
           #:*globally-redirect-io*
           #:*global-debugger*
           #:*sldb-printer-bindings*
           #:*swank-pprint-bindings*
           #:*default-worker-thread-bindings*
           #:*macroexpand-printer-bindings*
           #:*record-repl-results*
           #:*debug-on-swank-error*
           ;; These are re-exported directly from the backend:
           #:buffer-first-change
           #:frame-source-location-for-emacs
           #:restart-frame
           #:sldb-step 
           #:sldb-break
           #:sldb-break-on-return
           #:profiled-functions
           #:profile-report
           #:profile-reset
           #:unprofile-all
           #:profile-package
           #:default-directory
           #:set-default-directory
           #:quit-lisp))

(in-package :swank)


;;;; Top-level variables, constants, macros

(defconstant cl-package (find-package :cl)
  "The COMMON-LISP package.")

(defconstant keyword-package (find-package :keyword)
  "The KEYWORD package.")

(defvar *canonical-package-nicknames*
  `((:common-lisp-user . :cl-user))
  "Canonical package names to use instead of shortest name/nickname.")

(defvar *auto-abbreviate-dotted-packages* t
  "Abbreviate dotted package names to their last component if T.")

(defvar *swank-io-package*
  (let ((package (make-package :swank-io-package :use '())))
    (import '(nil t quote) package)
    package))

(defconstant default-server-port 4005
  "The default TCP port for the server (when started manually).")

(defvar *swank-debug-p* t
  "When true, print extra debugging information.")

(defvar *redirect-io* t
  "When non-nil redirect Lisp standard I/O to Emacs.
Redirection is done while Lisp is processing a request for Emacs.")

(defvar *sldb-printer-bindings*
  `((*print-pretty*           . t)
    (*print-level*            . 4)
    (*print-length*           . 10)
    (*print-circle*           . t)
    (*print-readably*         . nil)
    (*print-pprint-dispatch*  . ,(copy-pprint-dispatch nil))
    (*print-gensym*           . t)
    (*print-base*             . 10)
    (*print-radix*            . nil)
    (*print-array*            . t)
    (*print-lines*            . 10)
    (*print-escape*           . t)
    (*print-right-margin*     . 65))
  "A set of printer variables used in the debugger.")

(defvar *backtrace-pprint-dispatch-table*
  (let ((table (copy-pprint-dispatch nil)))
    (flet ((escape-string (stream string)
             (cond (*print-escape* 
                    (write-char #\" stream)
                    (loop for c across string do
                          (case c
                            (#\" (write-string "\\\"" stream))
                            (#\newline (write-string "\\n" stream))
                            (#\return (write-string "\\r" stream))
                            (t (write-char c stream))))
                    (write-char #\" stream))
                   (t (write-string string)))))
      (set-pprint-dispatch 'string  #'escape-string 0 table)
      table)))

(defvar *backtrace-printer-bindings*
  `((*print-pretty*           . t)
    (*print-readably*         . nil)
    (*print-level*            . 4)
    (*print-length*           . 6)
    (*print-lines*            . 1)
    (*print-right-margin*     . 200)
    (*print-pprint-dispatch*  . ,*backtrace-pprint-dispatch-table*))
  "Pretter settings for printing backtraces.")

(defvar *default-worker-thread-bindings* '()
  "An alist to initialize dynamic variables in worker threads.  
The list has the form ((VAR . VALUE) ...).  Each variable VAR will be
bound to the corresponding VALUE.")

(defun call-with-bindings (alist fun)
  "Call FUN with variables bound according to ALIST.
ALIST is a list of the form ((VAR . VAL) ...)."
  (let* ((rlist (reverse alist))
         (vars (mapcar #'car rlist))
         (vals (mapcar #'cdr rlist)))
    (progv vars vals
      (funcall fun))))

(defmacro with-bindings (alist &body body)
  "See `call-with-bindings'."
  `(call-with-bindings ,alist (lambda () ,@body)))

;;; The `DEFSLIMEFUN' macro defines a function that Emacs can call via
;;; RPC.

(defmacro defslimefun (name arglist &body rest)
  "A DEFUN for functions that Emacs can call by RPC."
  `(progn
     (defun ,name ,arglist ,@rest)
     ;; see <http://www.franz.com/support/documentation/6.2/doc/pages/variables/compiler/s_cltl1-compile-file-toplevel-compatibility-p_s.htm>
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name (symbol-package ',name)))))

(defun missing-arg ()
  "A function that the compiler knows will never to return a value.
You can use (MISSING-ARG) as the initform for defstruct slots that
must always be supplied. This way the :TYPE slot option need not
include some arbitrary initial value like NIL."
  (error "A required &KEY or &OPTIONAL argument was not supplied."))


;;;; Hooks
;;;
;;; We use Emacs-like `add-hook' and `run-hook' utilities to support
;;; simple indirection. The interface is more CLish than the Emacs
;;; Lisp one.

(defmacro add-hook (place function)
  "Add FUNCTION to the list of values on PLACE."
  `(pushnew ,function ,place))

(defun run-hook (functions &rest arguments)
  "Call each of FUNCTIONS with ARGUMENTS."
  (dolist (function functions)
    (apply function arguments)))

(defvar *new-connection-hook* '()
  "This hook is run each time a connection is established.
The connection structure is given as the argument.
Backend code should treat the connection structure as opaque.")

(defvar *connection-closed-hook* '()
  "This hook is run when a connection is closed.
The connection as passed as an argument.
Backend code should treat the connection structure as opaque.")

(defvar *pre-reply-hook* '()
  "Hook run (without arguments) immediately before replying to an RPC.")

(defvar *after-init-hook* '()
  "Hook run after user init files are loaded.")


;;;; Connections
;;;
;;; Connection structures represent the network connections between
;;; Emacs and Lisp. Each has a socket stream, a set of user I/O
;;; streams that redirect to Emacs, and optionally a second socket
;;; used solely to pipe user-output to Emacs (an optimization).
;;;

(defstruct (connection
             (:conc-name connection.)
             (:print-function print-connection))
  ;; Raw I/O stream of socket connection.
  (socket-io        (missing-arg) :type stream :read-only t)
  ;; Optional dedicated output socket (backending `user-output' slot).
  ;; Has a slot so that it can be closed with the connection.
  (dedicated-output nil :type (or stream null))
  ;; Streams that can be used for user interaction, with requests
  ;; redirected to Emacs.
  (user-input       nil :type (or stream null))
  (user-output      nil :type (or stream null))
  (user-io          nil :type (or stream null))
  ;; A stream that we use for *trace-output*; if nil, we user user-output.
  (trace-output     nil :type (or stream null))
  ;; A stream where we send REPL results.
  (repl-results     nil :type (or stream null))
  ;; In multithreaded systems we delegate certain tasks to specific
  ;; threads. The `reader-thread' is responsible for reading network
  ;; requests from Emacs and sending them to the `control-thread'; the
  ;; `control-thread' is responsible for dispatching requests to the
  ;; threads that should handle them; the `repl-thread' is the one
  ;; that evaluates REPL expressions. The control thread dispatches
  ;; all REPL evaluations to the REPL thread and for other requests it
  ;; spawns new threads.
  reader-thread
  control-thread
  repl-thread
  ;; Callback functions:
  ;; (SERVE-REQUESTS <this-connection>) serves all pending requests
  ;; from Emacs.
  (serve-requests   (missing-arg) :type function)
  ;; (CLEANUP <this-connection>) is called when the connection is
  ;; closed.
  (cleanup          nil :type (or null function))
  ;; Cache of macro-indentation information that has been sent to Emacs.
  ;; This is used for preparing deltas to update Emacs's knowledge.
  ;; Maps: symbol -> indentation-specification
  (indentation-cache (make-hash-table :test 'eq) :type hash-table)
  ;; The list of packages represented in the cache:
  (indentation-cache-packages '())
  ;; The communication style used.
  (communication-style nil :type (member nil :spawn :sigio :fd-handler))
  ;; The coding system for network streams.
  coding-system
  ;; The SIGINT handler we should restore when the connection is
  ;; closed.
  saved-sigint-handler)

(defun print-connection (conn stream depth)
  (declare (ignore depth))
  (print-unreadable-object (conn stream :type t :identity t)))

(defvar *connections* '()
  "List of all active connections, with the most recent at the front.")

(defvar *emacs-connection* nil
  "The connection to Emacs currently in use.")

(defvar *swank-state-stack* '()
  "A list of symbols describing the current state.  Used for debugging
and to detect situations where interrupts can be ignored.")

(defun default-connection ()
  "Return the 'default' Emacs connection.
This connection can be used to talk with Emacs when no specific
connection is in use, i.e. *EMACS-CONNECTION* is NIL.

The default connection is defined (quite arbitrarily) as the most
recently established one."
  (first *connections*))

(defslimefun state-stack ()
  "Return the value of *SWANK-STATE-STACK*."
  *swank-state-stack*)

(defslimefun ping (tag)
  tag)

;; A conditions to include backtrace information
(define-condition swank-error (error) 
  ((condition :initarg :condition :reader swank-error.condition)
   (backtrace :initarg :backtrace :reader swank-error.backtrace))
  (:report (lambda (condition stream)
             (princ (swank-error.condition condition) stream))))

(defun make-swank-error (condition)
  (make-condition 'swank-error :condition condition 
                  :backtrace (safe-backtrace)))

(defun safe-backtrace ()
  (ignore-errors 
    (call-with-debugging-environment 
     (lambda () (backtrace 0 nil)))))

(defvar *debug-on-swank-error* nil
  "When non-nil invoke the system debugger on swank internal errors.
Do not set this to T unless you want to debug swank internals.")

(defmacro with-swank-error-handler ((connection) &body body)
  (let ((var (gensym)))
  `(let ((,var ,connection))
     (handler-case 
         (handler-bind ((swank-error 
                         (lambda (condition)
                           (when *debug-on-swank-error*
                             (invoke-default-debugger condition)))))
           (progn ,@body))
       (swank-error (condition)
         (close-connection ,var
                           (swank-error.condition condition)
                           (swank-error.backtrace condition)))))))
  
(defmacro with-panic-handler ((connection) &body body)
  (let ((var (gensym)))
  `(let ((,var ,connection))
     (handler-bind ((serious-condition
                     (lambda (condition)
                       (close-connection ,var condition (safe-backtrace)))))
       . ,body))))

(add-hook *new-connection-hook* 'notify-backend-of-connection)
(defun notify-backend-of-connection (connection)
  (declare (ignore connection))
  (emacs-connected))


;;;; Utilities

;;;;; Helper macros

;; If true execute interrupts, otherwise queue them.
;; Note: `with-connection' binds *pending-slime-interrupts*.
(defvar *slime-interrupts-enabled*)

(defmacro with-interrupts-enabled% (flag body)
  `(progn
     (check-slime-interrupts)
     (multiple-value-prog1
         (let ((*slime-interrupts-enabled* ,flag))
           ,@body)
       (check-slime-interrupts))))

(defmacro with-slime-interrupts (&body body)
  `(with-interrupts-enabled% t ,body))

(defmacro without-slime-interrupts (&body body)
  `(with-interrupts-enabled% nil ,body))

(defun invoke-or-queue-interrupt (function)
  (log-event "invoke-or-queue-interrupt: ~a" function)
  (cond ((not (boundp '*slime-interrupts-enabled*))
         (without-slime-interrupts
           (funcall function)))
        (*slime-interrupts-enabled*
         (funcall function))
        (t
         (setq *pending-slime-interrupts*
               (nconc *pending-slime-interrupts*
                      (list function)))
         (cond ((cdr *pending-slime-interrupts*)
                (check-slime-interrupts))
               (t
                (log-event "queue-interrupt: ~a" function))))))

(defslimefun simple-break (&optional (datum "Interrupt from Emacs") &rest args)
  (with-simple-restart (continue "Continue from break.")
    (invoke-slime-debugger (coerce-to-condition datum args))))

(defun coerce-to-condition (datum args)
  (etypecase datum
    (string (make-condition 'simple-error :format-control datum 
                            :format-arguments args))
    (symbol (apply #'make-condition datum args))))

(defmacro with-io-redirection ((connection) &body body)
  "Execute BODY I/O redirection to CONNECTION.
If *REDIRECT-IO* is true then all standard I/O streams are redirected."
  `(maybe-call-with-io-redirection ,connection (lambda () ,@body)))

(defun maybe-call-with-io-redirection (connection fun)
  (if *redirect-io*
      (call-with-redirected-io connection fun)
      (funcall fun)))
      
(defmacro with-connection ((connection) &body body)
  "Execute BODY in the context of CONNECTION."
  `(call-with-connection ,connection (lambda () ,@body)))

(defun call-with-connection (connection function)
  (if (eq *emacs-connection* connection)
      (funcall function)
      (let ((*emacs-connection* connection)
            (*pending-slime-interrupts* '()))
        (without-slime-interrupts
          (with-swank-error-handler (*emacs-connection*)
            (with-io-redirection (*emacs-connection*)
              (call-with-debugger-hook #'swank-debugger-hook function)))))))

(defun call-with-retry-restart (msg thunk)
  (let ((%ok    (gensym "OK+"))
	(%retry (gensym "RETRY+")))
    (restart-bind
	((retry
	  (lambda () (throw %retry nil))
	   :report-function
	   (lambda (stream)
	     (write msg :stream stream))))
      (catch %ok
	(loop (catch %retry (throw %ok (funcall thunk))))))))

(defmacro with-retry-restart ((&key (msg "Retry.")) &body body)
  (check-type msg string)
  `(call-with-retry-restart ,msg #'(lambda () ,@body)))

;;; FIXME: Can this be removed with the introduction of
;;;        WITH/WITHOUT-SLIME-INTERRUPTS.
(defmacro without-interrupts (&body body)
  `(call-without-interrupts (lambda () ,@body)))

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
         ,@(loop for (pattern . body) in patterns collect 
                   (if (eq pattern t)
                       `(t ,@body)
                       (destructuring-bind (op &rest rands) pattern
                         `(,op (destructuring-bind ,rands ,operands 
                                 ,@body)))))
         ,@(if (eq (caar (last patterns)) t)
               '()
               `((t (error "destructure-case failed: ~S" ,tmp))))))))

(defmacro with-struct* ((conc-name get obj) &body body)
  (let ((var (gensym)))
    `(let ((,var ,obj))
       (macrolet ((,get (slot)
                    (let ((getter (intern (concatenate 'string
                                                       ',(string conc-name)
                                                       (string slot))
                                          (symbol-package ',conc-name))))
                      `(,getter ,',var))))
         ,@body))))

(defmacro with-temp-package (var &body body)
  "Execute BODY with VAR bound to a temporary package.
The package is deleted before returning."
  `(let ((,var (make-package (gensym "TEMP-PACKAGE-"))))
     (unwind-protect (progn ,@body)
       (delete-package ,var))))

(defmacro do-symbols* ((var &optional (package '*package*) result-form) &body body)
  "Just like do-symbols, but makes sure a symbol is visited only once."
  (let ((seen-ht (gensym "SEEN-HT")))
    `(let ((,seen-ht (make-hash-table :test #'eq)))
      (do-symbols (,var ,package ,result-form)
        (unless (gethash ,var ,seen-ht)
          (setf (gethash ,var ,seen-ht) t)
          ,@body)))))

(defun use-threads-p ()
  (eq (connection.communication-style *emacs-connection*) :spawn))

(defun current-thread-id ()
  (thread-id (current-thread)))



;;;;; Logging

(defvar *log-events* nil)
(defvar *log-output* nil) ; should be nil for image dumpers

(defun init-log-output ()
  (labels ((deref (x)
             (cond ((typep x 'synonym-stream)
                    (deref (symbol-value (synonym-stream-symbol x))))
                   (t x))))
    (unless *log-output*
      (setq *log-output* (deref *error-output*)))))

(add-hook *after-init-hook* 'init-log-output)

(defvar *event-history* (make-array 40 :initial-element nil)
  "A ring buffer to record events for better error messages.")
(defvar *event-history-index* 0)
(defvar *enable-event-history* t)

(defun log-event (format-string &rest args)
  "Write a message to *terminal-io* when *log-events* is non-nil.
Useful for low level debugging."
  (with-standard-io-syntax
    (let ((*print-readably* nil)
          (*print-pretty* nil)
          (*package* *swank-io-package*))
      (when *enable-event-history*
        (setf (aref *event-history* *event-history-index*) 
              (format nil "~?" format-string args))
        (setf *event-history-index* 
              (mod (1+ *event-history-index*) (length *event-history*))))
      (when *log-events*
        (write-string (escape-non-ascii (format nil "~?" format-string args))
                      *log-output*)
        (force-output *log-output*)))))

(defun event-history-to-list ()
  "Return the list of events (older events first)."
  (let ((arr *event-history*)
        (idx *event-history-index*))
    (concatenate 'list (subseq arr idx) (subseq arr 0 idx))))

(defun dump-event-history (stream)
  (dolist (e (event-history-to-list))
    (dump-event e stream)))

(defun dump-event (event stream)
  (cond ((stringp event)
         (write-string (escape-non-ascii event) stream))
        ((null event))
        (t 
         (write-string
          (escape-non-ascii (format nil "Unexpected event: ~A~%" event))
          stream))))

(defun escape-non-ascii (string)
  "Return a string like STRING but with non-ascii chars escaped."
  (cond ((ascii-string-p string) string)
        (t (with-output-to-string (out)
             (loop for c across string do
               (cond ((ascii-char-p c) (write-char c out))
                     (t (format out "\\x~4,'0X" (char-code c)))))))))

(defun ascii-string-p (o)
  (and (stringp o)
       (every #'ascii-char-p o)))

(defun ascii-char-p (c) 
  (<= (char-code c) 127))


;;;;; Symbols

(defun symbol-status (symbol &optional (package (symbol-package symbol)))
  "Returns one of 

  :INTERNAL  if the symbol is _present_ in PACKAGE as an _internal_ symbol,

  :EXTERNAL  if the symbol is _present_ in PACKAGE as an _external_ symbol,

  :INHERITED if the symbol is _inherited_ by PACKAGE through USE-PACKAGE,
             but is not _present_ in PACKAGE,

  or NIL     if SYMBOL is not _accessible_ in PACKAGE.


Be aware not to get confused with :INTERNAL and how \"internal
symbols\" are defined in the spec; there is a slight mismatch of
definition with the Spec and what's commonly meant when talking
about internal symbols most times. As the spec says:

  In a package P, a symbol S is
  
     _accessible_  if S is either _present_ in P itself or was
                   inherited from another package Q (which implies
                   that S is _external_ in Q.)
  
        You can check that with: (AND (SYMBOL-STATUS S P) T)
  
  
     _present_     if either P is the /home package/ of S or S has been
                   imported into P or exported from P by IMPORT, or
                   EXPORT respectively.
  
                   Or more simply, if S is not _inherited_.
  
        You can check that with: (LET ((STATUS (SYMBOL-STATUS S P)))
                                   (AND STATUS 
                                        (NOT (EQ STATUS :INHERITED))))
  
  
     _external_    if S is going to be inherited into any package that
                   /uses/ P by means of USE-PACKAGE, MAKE-PACKAGE, or
                   DEFPACKAGE.
  
                   Note that _external_ implies _present_, since to
                   make a symbol _external_, you'd have to use EXPORT
                   which will automatically make the symbol _present_.
  
        You can check that with: (EQ (SYMBOL-STATUS S P) :EXTERNAL)
  
  
     _internal_    if S is _accessible_ but not _external_.

        You can check that with: (LET ((STATUS (SYMBOL-STATUS S P)))
                                   (AND STATUS 
                                        (NOT (EQ STATUS :EXTERNAL))))
  

        Notice that this is *different* to
                                 (EQ (SYMBOL-STATUS S P) :INTERNAL)
        because what the spec considers _internal_ is split up into two
        explicit pieces: :INTERNAL, and :INHERITED; just as, for instance,
        CL:FIND-SYMBOL does. 

        The rationale is that most times when you speak about \"internal\"
        symbols, you're actually not including the symbols inherited 
        from other packages, but only about the symbols directly specific
        to the package in question.
"
  (when package     ; may be NIL when symbol is completely uninterned.
    (check-type symbol symbol) (check-type package package)
    (multiple-value-bind (present-symbol status)
        (find-symbol (symbol-name symbol) package)
      (and (eq symbol present-symbol) status))))

(defun symbol-external-p (symbol &optional (package (symbol-package symbol)))
  "True if SYMBOL is external in PACKAGE.
If PACKAGE is not specified, the home package of SYMBOL is used."
  (eq (symbol-status symbol package) :external))


(defun classify-symbol (symbol)
  "Returns a list of classifiers that classify SYMBOL according to its
underneath objects (e.g. :BOUNDP if SYMBOL constitutes a special
variable.) The list may contain the following classification
keywords: :BOUNDP, :FBOUNDP, :CONSTANT, :GENERIC-FUNCTION,
:TYPESPEC, :CLASS, :MACRO, :SPECIAL-OPERATOR, and/or :PACKAGE"
  (check-type symbol symbol)
  (flet ((type-specifier-p (s)
           (or (documentation s 'type)
               (not (eq (type-specifier-arglist s) :not-available)))))
    (let (result)
      (when (boundp symbol)             (push (if (constantp symbol)
                                                  :constant :boundp) result))
      (when (fboundp symbol)            (push :fboundp result))
      (when (type-specifier-p symbol)   (push :typespec result))
      (when (find-class symbol nil)     (push :class result))
      (when (macro-function symbol)     (push :macro result))
      (when (special-operator-p symbol) (push :special-operator result))
      (when (find-package symbol)       (push :package result))
      (when (typep (ignore-errors (fdefinition symbol))
                   'generic-function)
        (push :generic-function result))

      result)))

(defun symbol-classification->string (flags)
  (format nil "~A~A~A~A~A~A~A~A"
          (if (or (member :boundp flags)
                  (member :constant flags)) "b" "-")
          (if (member :fboundp flags) "f" "-")
          (if (member :generic-function flags) "g" "-")
          (if (member :class flags) "c" "-")
          (if (member :typespec flags) "t" "-")
          (if (member :macro flags) "m" "-")
          (if (member :special-operator flags) "s" "-")
          (if (member :package flags) "p" "-")))


;;;; TCP Server

(defvar *use-dedicated-output-stream* nil
  "When T swank will attempt to create a second connection to
  Emacs which is used just to send output.")

(defvar *dedicated-output-stream-port* 0
  "Which port we should use for the dedicated output stream.")

(defvar *communication-style* (preferred-communication-style))

(defvar *dont-close* nil
  "Default value of :dont-close argument to start-server and
  create-server.")

(defvar *dedicated-output-stream-buffering* 
  (if (eq *communication-style* :spawn) :full :none)
  "The buffering scheme that should be used for the output stream.
Valid values are :none, :line, and :full.")

(defvar *coding-system* "iso-latin-1-unix")

(defvar *listener-sockets* nil
  "A property list of lists containing style, socket pairs used 
   by swank server listeners, keyed on socket port number. They 
   are used to close sockets on server shutdown or restart.")

(defun start-server (port-file &key (style *communication-style*)
                                    (dont-close *dont-close*)
                                    (coding-system *coding-system*))
  "Start the server and write the listen port number to PORT-FILE.
This is the entry point for Emacs."
  (setup-server 0 (lambda (port) 
                    (announce-server-port port-file port))
                style dont-close 
                (find-external-format-or-lose coding-system)))

(defun create-server (&key (port default-server-port)
                      (style *communication-style*)
                      (dont-close *dont-close*) 
                      (coding-system *coding-system*))
  "Start a SWANK server on PORT running in STYLE.
If DONT-CLOSE is true then the listen socket will accept multiple
connections, otherwise it will be closed after the first."
  (setup-server port #'simple-announce-function style dont-close 
                (find-external-format-or-lose coding-system)))

(defun find-external-format-or-lose (coding-system)
  (or (find-external-format coding-system)
      (error "Unsupported coding system: ~s" coding-system)))

(defparameter *loopback-interface* "127.0.0.1")

(defun setup-server (port announce-fn style dont-close external-format)
  (declare (type function announce-fn))
  (init-log-output)
  (let* ((socket (create-socket *loopback-interface* port))
         (local-port (local-port socket)))
    (funcall announce-fn local-port)
    (flet ((serve ()
             (serve-connection socket style dont-close external-format)))
      (ecase style
        (:spawn
         (initialize-multiprocessing
          (lambda ()
            (spawn (lambda () 
                     (cond ((not dont-close) (serve))
                           (t (loop (ignore-errors (serve))))))
                   :name (cat "Swank " (princ-to-string port))))))
        ((:fd-handler :sigio)
         (add-fd-handler socket (lambda () (serve))))
        ((nil) (loop do (serve) while dont-close)))
      (setf (getf *listener-sockets* port) (list style socket))
      local-port)))

(defun stop-server (port)
  "Stop server running on PORT."
  (let* ((socket-description (getf *listener-sockets* port))
         (style (first socket-description))
         (socket (second socket-description)))
    (ecase style
      (:spawn
       (let ((thread-position
              (position-if 
               (lambda (x) 
                 (string-equal (first x)
                               (concatenate 'string "Swank "
                                            (princ-to-string port))))
               (list-threads))))
         (when thread-position
           (kill-nth-thread thread-position)
           (close-socket socket)
           (remf *listener-sockets* port))))
      ((:fd-handler :sigio)
       (remove-fd-handlers socket)
       (close-socket socket)
       (remf *listener-sockets* port)))))

(defun restart-server (&key (port default-server-port)
                       (style *communication-style*)
                       (dont-close *dont-close*) 
                       (coding-system *coding-system*))
  "Stop the server listening on PORT, then start a new SWANK server 
on PORT running in STYLE. If DONT-CLOSE is true then the listen socket 
will accept multiple connections, otherwise it will be closed after the 
first."
  (stop-server port)
  (sleep 5)
  (create-server :port port :style style :dont-close dont-close
                 :coding-system coding-system))


(defun serve-connection (socket style dont-close external-format)
  (let ((closed-socket-p nil))
    (unwind-protect
         (let ((client (accept-authenticated-connection
                        socket :external-format external-format)))
           (unless dont-close
             (close-socket socket)
             (setf closed-socket-p t))
           (let ((connection (create-connection client style)))
             (run-hook *new-connection-hook* connection)
             (push connection *connections*)
             (serve-requests connection)))
      (unless (or dont-close closed-socket-p)
        (close-socket socket)))))

(defun accept-authenticated-connection (&rest args)
  (let ((new (apply #'accept-connection args))
        (success nil))
    (unwind-protect
         (let ((secret (slime-secret)))
           (when secret
             (set-stream-timeout new 20)
             (let ((first-val (decode-message new)))
               (unless (and (stringp first-val) (string= first-val secret))
                 (error "Incoming connection doesn't know the password."))))
           (set-stream-timeout new nil)
           (setf success t))
      (unless success
        (close new :abort t)))
    new))

(defun slime-secret ()
  "Finds the magic secret from the user's home directory.  Returns nil
if the file doesn't exist; otherwise the first line of the file."
  (with-open-file (in
                   (merge-pathnames (user-homedir-pathname) #p".slime-secret")
                   :if-does-not-exist nil)
    (and in (read-line in nil ""))))

(defun serve-requests (connection)
  "Read and process all requests on connections."
  (funcall (connection.serve-requests connection) connection))

(defun announce-server-port (file port)
  (with-open-file (s file
                     :direction :output
                     :if-exists :error
                     :if-does-not-exist :create)
    (format s "~S~%" port))
  (simple-announce-function port))

(defun simple-announce-function (port)
  (when *swank-debug-p*
    (format *log-output* "~&;; Swank started at port: ~D.~%" port)
    (force-output *log-output*)))

(defun open-streams (connection)
  "Return the 5 streams for IO redirection:
DEDICATED-OUTPUT INPUT OUTPUT IO REPL-RESULTS"
  (let* ((output-fn (make-output-function connection))
         (input-fn
          (lambda () 
            (with-connection (connection)
              (with-simple-restart (abort-read
                                    "Abort reading input from Emacs.")
                (read-user-input-from-emacs)))))
         (dedicated-output (if *use-dedicated-output-stream*
                               (open-dedicated-output-stream
                                (connection.socket-io connection))))
         (out (make-output-stream output-fn))
         (in (make-input-stream input-fn))
         (out (or dedicated-output out))
         (io (make-two-way-stream in out))
         (repl-results (make-output-stream-for-target connection
                                                      :repl-result)))
    (when (eq (connection.communication-style connection) :spawn)
      (spawn (lambda () (auto-flush-loop out))
             :name "auto-flush-thread"))
    (values dedicated-output in out io repl-results)))

;; FIXME: if wait-for-event aborts the event will stay in the queue forever.
(defun make-output-function (connection)
  "Create function to send user output to Emacs."
  (let ((i 0) (tag 0) (l 0))
    (lambda (string)
      (with-connection (connection)
        (multiple-value-setq (i tag l) 
          (send-user-output string i tag l))))))

(defvar *maximum-pipelined-output-chunks* 50)
(defvar *maximum-pipelined-output-length* (* 80 20 5))
(defun send-user-output (string pcount tag plength)
  ;; send output with flow control
  (when (or (> pcount *maximum-pipelined-output-chunks*) 
            (> plength *maximum-pipelined-output-length*))
    (setf tag (mod (1+ tag) 1000))
    (send-to-emacs `(:ping ,(current-thread-id) ,tag))
    (with-simple-restart (abort "Abort sending output to Emacs.")
      (wait-for-event `(:emacs-pong ,tag)))
    (setf pcount 0) 
    (setf plength 0))
  (send-to-emacs `(:write-string ,string))
  (values (1+ pcount) tag (+ plength (length string))))

(defun make-output-function-for-target (connection target)
  "Create a function to send user output to a specific TARGET in Emacs."
  (lambda (string) 
    (with-connection (connection)
      (with-simple-restart
          (abort "Abort sending output to Emacs.")
        (send-to-emacs `(:write-string ,string ,target))))))

(defun make-output-stream-for-target (connection target)
  "Create a stream that sends output to a specific TARGET in Emacs."
  (make-output-stream (make-output-function-for-target connection target)))

(defun open-dedicated-output-stream (socket-io)
  "Open a dedicated output connection to the Emacs on SOCKET-IO.
Return an output stream suitable for writing program output.

This is an optimized way for Lisp to deliver output to Emacs."
  (let ((socket (create-socket *loopback-interface* 
                               *dedicated-output-stream-port*)))
    (unwind-protect
         (let ((port (local-port socket)))
           (encode-message `(:open-dedicated-output-stream ,port) socket-io)
           (let ((dedicated (accept-authenticated-connection 
                             socket 
                             :external-format 
                             (or (ignore-errors
                                   (stream-external-format socket-io))
                                 :default)
                             :buffering *dedicated-output-stream-buffering*
                             :timeout 30)))
             (close-socket socket)
             (setf socket nil)
             dedicated))
      (when socket
        (close-socket socket)))))

;; The restart that will be invoked when the user calls sldb-quit.
;; This restart will be named "abort" because many people press "a"
;; instead of "q" in the debugger.
(defvar *sldb-quit-restart*)

;; Establish a top-level restart and execute BODY.
;; Execute K if the restart is invoked.
(defmacro with-top-level-restart ((connection k) &body body)
  `(with-connection (,connection)
     (restart-case 
         (let ((*sldb-quit-restart* (find-restart 'abort)))
           . ,body)
       (abort (&optional v)
         :report "Return to SLIME's top level."
         (declare (ignore v))
         (force-user-output)
         ,k))))

(defun handle-requests (connection &optional timeout)
  "Read and process :emacs-rex requests.
The processing is done in the extent of the toplevel restart."
  (cond ((boundp '*sldb-quit-restart*)
         (process-requests timeout))
        (t 
         (tagbody
            start
            (with-top-level-restart (connection (go start))
              (process-requests timeout))))))

(defun process-requests (timeout)
  "Read and process requests from Emacs."
  (loop
   (multiple-value-bind (event timeout?)
       (wait-for-event `(:emacs-rex . _) timeout)
    (when timeout? (return))
    (apply #'eval-for-emacs (cdr event)))))

(defun current-socket-io ()
  (connection.socket-io *emacs-connection*))

(defun close-connection (c condition backtrace)
  (let ((*debugger-hook* nil))
    (log-event "close-connection: ~a ...~%" condition)
  (format *log-output* "~&;; swank:close-connection: ~A~%" condition)
  (let ((cleanup (connection.cleanup c)))
    (when cleanup
      (funcall cleanup c)))
  (close (connection.socket-io c))
  (when (connection.dedicated-output c)
    (close (connection.dedicated-output c)))
  (setf *connections* (remove c *connections*))
  (run-hook *connection-closed-hook* c)
  (when (and condition (not (typep condition 'end-of-file)))
    (finish-output *log-output*)
    (format *log-output* "~&;; Event history start:~%")
    (dump-event-history *log-output*)
    (format *log-output* ";; Event history end.~%~
                        ;; Backtrace:~%~{~A~%~}~
                        ;; Connection to Emacs lost. [~%~
                        ;;  condition: ~A~%~
                        ;;  type: ~S~%~
                        ;;  encoding: ~A style: ~S dedicated: ~S]~%"
            backtrace
            (escape-non-ascii (safe-condition-message condition) )
            (type-of condition)
            (ignore-errors (stream-external-format (connection.socket-io c)))
            (connection.communication-style c)
            *use-dedicated-output-stream*)
    (finish-output *log-output*))
  (log-event "close-connection ~a ... done.~%" condition)))

;;;;;; Thread based communication

(defvar *active-threads* '())

(defun read-loop (connection)
  (let ((input-stream (connection.socket-io connection))
        (control-thread (connection.control-thread connection)))
    (with-swank-error-handler (connection)
      (loop (send control-thread (decode-message input-stream))))))

(defun dispatch-loop (connection)
  (let ((*emacs-connection* connection))
    (with-panic-handler (connection)
      (loop (dispatch-event (receive))))))

(defvar *auto-flush-interval* 0.2)

(defun auto-flush-loop (stream)
  (loop
   (when (not (and (open-stream-p stream) 
                   (output-stream-p stream)))
     (return nil))
   (finish-output stream)
   (sleep *auto-flush-interval*)))

(defun find-repl-thread (connection)
  (cond ((not (use-threads-p))
         (current-thread))
        (t
         (let ((thread (connection.repl-thread connection)))
           (assert thread)
           (cond ((thread-alive-p thread) thread)
                 (t
                  (setf (connection.repl-thread connection)
                        (spawn-repl-thread connection "new-repl-thread"))))))))

(defun find-worker-thread (id)
  (etypecase id
    ((member t)
     (car *active-threads*))
    ((member :repl-thread) 
     (find-repl-thread *emacs-connection*))
    (fixnum 
     (find-thread id))))

(defun interrupt-worker-thread (id)
  (let ((thread (or (find-worker-thread id)
                    (find-repl-thread *emacs-connection*))))
    (signal-interrupt thread
                      (lambda () 
                        (invoke-or-queue-interrupt #'simple-break)))))

(defun thread-for-evaluation (id)
  "Find or create a thread to evaluate the next request."
  (let ((c *emacs-connection*))
    (etypecase id
      ((member t)
       (cond ((use-threads-p) (spawn-worker-thread c))
             (t (current-thread))))
      ((member :repl-thread)
       (find-repl-thread c))
      (fixnum
       (find-thread id)))))

(defun spawn-worker-thread (connection)
  (spawn (lambda () 
           (with-bindings *default-worker-thread-bindings*
             (with-top-level-restart (connection nil)
               (apply #'eval-for-emacs 
                      (cdr (wait-for-event `(:emacs-rex . _)))))))
         :name "worker"))

(defun spawn-repl-thread (connection name)
  (spawn (lambda () 
           (with-bindings *default-worker-thread-bindings*
             (repl-loop connection)))
         :name name))

(defun dispatch-event (event)
  "Handle an event triggered either by Emacs or within Lisp."
  (log-event "dispatch-event: ~s~%" event)
  (destructure-case event
    ((:emacs-rex form package thread-id id)
     (let ((thread (thread-for-evaluation thread-id)))
       (push thread *active-threads*)
       (send-event thread `(:emacs-rex ,form ,package ,id))))
    ((:return thread &rest args)
     (let ((tail (member thread *active-threads*)))
       (setq *active-threads* (nconc (ldiff *active-threads* tail)
				     (cdr tail))))
     (encode-message `(:return ,@args) (current-socket-io)))
    ((:emacs-interrupt thread-id)
     (interrupt-worker-thread thread-id))
    (((:write-string
       :debug :debug-condition :debug-activate :debug-return
       :presentation-start :presentation-end
       :new-package :new-features :ed :%apply :indentation-update
       :eval :eval-no-wait :background-message :inspect :ping
       :y-or-n-p :read-string :read-aborted)
      &rest _)
     (declare (ignore _))
     (encode-message event (current-socket-io)))
    (((:emacs-pong :emacs-return :emacs-return-string) thread-id &rest args)
     (send-event (find-thread thread-id) (cons (car event) args)))
    (((:end-of-stream))
     (close-connection *emacs-connection* nil (safe-backtrace)))
    ((:reader-error packet condition)
     (encode-message `(:reader-error ,packet 
                                     ,(safe-condition-message condition))
                     (current-socket-io)))))

(defvar *event-queue* '())
(defvar *events-enqueued* 0)

(defun send-event (thread event)
  (log-event "send-event: ~s ~s~%" thread event)
  (cond ((use-threads-p) (send thread event))
        (t (setf *event-queue* (nconc *event-queue* (list event)))
           (setf *events-enqueued* (mod (1+ *events-enqueued*)
                                        most-positive-fixnum)))))

(defun send-to-emacs (event)
  "Send EVENT to Emacs."
  ;;(log-event "send-to-emacs: ~a" event)
  (cond ((use-threads-p) 
         (send (connection.control-thread *emacs-connection*) event))
        (t (dispatch-event event))))

(defun signal-interrupt (thread interrupt)  
  (log-event "signal-interrupt~%")
  (cond ((use-threads-p) (interrupt-thread thread interrupt))
        (t (funcall interrupt))))

(defun wait-for-event (pattern &optional timeout)
  (log-event "wait-for-event: ~s ~s~%" pattern timeout)
  (without-slime-interrupts
    (cond ((use-threads-p) 
           (receive-if (lambda (e) (event-match-p e pattern)) timeout))
          (t 
           (wait-for-event/event-loop pattern timeout)))))

(defun wait-for-event/event-loop (pattern timeout)
  (assert (or (not timeout) (eq timeout t)))
  (loop 
   (check-slime-interrupts)
   (let ((event (poll-for-event pattern)))
     (when event (return (car event))))
   (let ((events-enqueued *events-enqueued*)
         (ready (wait-for-input (list (current-socket-io)) timeout)))
     (cond ((and timeout (not ready))
            (return (values nil t)))
           ((or (/= events-enqueued *events-enqueued*)
                (eq ready :interrupt))
            ;; rescan event queue, interrupts may enqueue new events 
            )
           (t
            (assert (equal ready (list (current-socket-io))))
            (dispatch-event (decode-message (current-socket-io))))))))

(defun poll-for-event (pattern)
  (let ((tail (member-if (lambda (e) (event-match-p e pattern))
                         *event-queue*)))
    (when tail 
      (setq *event-queue* (nconc (ldiff *event-queue* tail)
                                 (cdr tail)))
      tail)))

(defun event-match-p (event pattern)
  (cond ((or (keywordp pattern) (numberp pattern) (stringp pattern)
	     (member pattern '(nil t)))
	 (equal event pattern))
	((symbolp pattern) t)
	((consp pattern)
         (case (car pattern)
           ((or) (some (lambda (p) (event-match-p event p)) (cdr pattern)))
           (t (and (consp event)
                   (and (event-match-p (car event) (car pattern))
                        (event-match-p (cdr event) (cdr pattern)))))))
        (t (error "Invalid pattern: ~S" pattern))))

(defun spawn-threads-for-connection (connection)
  (setf (connection.control-thread connection) 
        (spawn (lambda () (control-thread connection))
               :name "control-thread"))
  connection)

(defun control-thread (connection)
  (with-struct* (connection. @ connection)
    (setf (@ control-thread) (current-thread))
    (setf (@ repl-thread) (spawn-repl-thread connection "repl-thread"))
    (setf (@ reader-thread) (spawn (lambda () (read-loop connection)) 
                                   :name "reader-thread"))
    (dispatch-loop connection)))

(defun cleanup-connection-threads (connection)
  (let ((threads (list (connection.repl-thread connection)
                       (connection.reader-thread connection)
                       (connection.control-thread connection))))
    (dolist (thread threads)
      (when (and thread 
                 (thread-alive-p thread)
                 (not (equal (current-thread) thread)))
        (kill-thread thread)))))

(defun repl-loop (connection)
  (handle-requests connection))

;;;;;; Signal driven IO

(defun install-sigio-handler (connection)
  (add-sigio-handler (connection.socket-io connection) 
                     (lambda () (process-io-interrupt connection)))
  (handle-requests connection t))

(defvar *io-interupt-level* 0)

(defun process-io-interrupt (connection)
  (log-event "process-io-interrupt ~d ...~%" *io-interupt-level*)
  (let ((*io-interupt-level* (1+ *io-interupt-level*)))
    (invoke-or-queue-interrupt
     (lambda () (handle-requests connection t))))
  (log-event "process-io-interrupt ~d ... done ~%" *io-interupt-level*))

(defun deinstall-sigio-handler (connection)
  (log-event "deinstall-sigio-handler...~%")
  (remove-sigio-handlers (connection.socket-io connection))
  (log-event "deinstall-sigio-handler...done~%"))

;;;;;; SERVE-EVENT based IO

(defun install-fd-handler (connection)
  (add-fd-handler (connection.socket-io connection)
                  (lambda () (handle-requests connection t)))
  (setf (connection.saved-sigint-handler connection)
        (install-sigint-handler 
         (lambda () 
           (invoke-or-queue-interrupt
            (lambda () 
              (with-connection (connection)
                (dispatch-interrupt-event)))))))
  (handle-requests connection t))

(defun dispatch-interrupt-event ()
  (dispatch-event `(:emacs-interrupt ,(current-thread-id))))

(defun deinstall-fd-handler (connection)
  (log-event "deinstall-fd-handler~%")
  (remove-fd-handlers (connection.socket-io connection))
  (install-sigint-handler (connection.saved-sigint-handler connection)))

;;;;;; Simple sequential IO

(defun simple-serve-requests (connection)
  (unwind-protect 
       (call-with-user-break-handler
        (lambda () 
          (invoke-or-queue-interrupt #'dispatch-interrupt-event))
        (lambda ()
          (with-simple-restart (close-connection "Close SLIME connection")
            (handle-requests connection))))
    (close-connection connection nil (safe-backtrace))))

(defun initialize-streams-for-connection (connection)
  (multiple-value-bind (dedicated in out io repl-results) 
      (open-streams connection)
    (setf (connection.dedicated-output connection) dedicated
          (connection.user-io connection)          io
          (connection.user-output connection)      out
          (connection.user-input connection)       in
          (connection.repl-results connection)     repl-results)
    connection))

(defun create-connection (socket-io style)
  (let ((success nil))
    (unwind-protect
         (let ((c (ecase style
                    (:spawn
                     (make-connection :socket-io socket-io
                                      :serve-requests #'spawn-threads-for-connection
                                      :cleanup #'cleanup-connection-threads))
                    (:sigio
                     (make-connection :socket-io socket-io
                                      :serve-requests #'install-sigio-handler
                                      :cleanup #'deinstall-sigio-handler))
                    (:fd-handler
                     (make-connection :socket-io socket-io
                                      :serve-requests #'install-fd-handler
                                      :cleanup #'deinstall-fd-handler))
                    ((nil)
                     (make-connection :socket-io socket-io
                                      :serve-requests #'simple-serve-requests))
                    )))
           (setf (connection.communication-style c) style)
           (initialize-streams-for-connection c)
           (setf success t)
           c)
      (unless success
        (close socket-io :abort t)))))


;;;; IO to Emacs
;;;
;;; This code handles redirection of the standard I/O streams
;;; (`*standard-output*', etc) into Emacs. The `connection' structure
;;; contains the appropriate streams, so all we have to do is make the
;;; right bindings.

;;;;; Global I/O redirection framework
;;;
;;; Optionally, the top-level global bindings of the standard streams
;;; can be assigned to be redirected to Emacs. When Emacs connects we
;;; redirect the streams into the connection, and they keep going into
;;; that connection even if more are established. If the connection
;;; handling the streams closes then another is chosen, or if there
;;; are no connections then we revert to the original (real) streams.
;;;
;;; It is slightly tricky to assign the global values of standard
;;; streams because they are often shadowed by dynamic bindings. We
;;; solve this problem by introducing an extra indirection via synonym
;;; streams, so that *STANDARD-INPUT* is a synonym stream to
;;; *CURRENT-STANDARD-INPUT*, etc. We never shadow the "current"
;;; variables, so they can always be assigned to affect a global
;;; change.

(defvar *globally-redirect-io* nil
  "When non-nil globally redirect all standard streams to Emacs.")

;;;;; Global redirection setup

(defvar *saved-global-streams* '()
  "A plist to save and restore redirected stream objects.
E.g. the value for '*standard-output* holds the stream object
for *standard-output* before we install our redirection.")

(defun setup-stream-indirection (stream-var &optional stream)
  "Setup redirection scaffolding for a global stream variable.
Supposing (for example) STREAM-VAR is *STANDARD-INPUT*, this macro:

1. Saves the value of *STANDARD-INPUT* in `*SAVED-GLOBAL-STREAMS*'.

2. Creates *CURRENT-STANDARD-INPUT*, initially with the same value as
*STANDARD-INPUT*.

3. Assigns *STANDARD-INPUT* to a synonym stream pointing to
*CURRENT-STANDARD-INPUT*.

This has the effect of making *CURRENT-STANDARD-INPUT* contain the
effective global value for *STANDARD-INPUT*. This way we can assign
the effective global value even when *STANDARD-INPUT* is shadowed by a
dynamic binding."
  (let ((current-stream-var (prefixed-var '#:current stream-var))
        (stream (or stream (symbol-value stream-var))))
    ;; Save the real stream value for the future.
    (setf (getf *saved-global-streams* stream-var) stream)
    ;; Define a new variable for the effective stream.
    ;; This can be reassigned.
    (proclaim `(special ,current-stream-var))
    (set current-stream-var stream)
    ;; Assign the real binding as a synonym for the current one.
    (set stream-var (make-synonym-stream current-stream-var))))

(defun prefixed-var (prefix variable-symbol)
  "(PREFIXED-VAR \"FOO\" '*BAR*) => SWANK::*FOO-BAR*"
  (let ((basename (subseq (symbol-name variable-symbol) 1)))
    (intern (format nil "*~A-~A" (string prefix) basename) :swank)))

(defvar *standard-output-streams*
  '(*standard-output* *error-output* *trace-output*)
  "The symbols naming standard output streams.")

(defvar *standard-input-streams*
  '(*standard-input*)
  "The symbols naming standard input streams.")

(defvar *standard-io-streams*
  '(*debug-io* *query-io* *terminal-io*)
  "The symbols naming standard io streams.")

(defun init-global-stream-redirection ()
  (when *globally-redirect-io*
    (assert (not *saved-global-streams*) () "Streams already redirected.")
    (mapc #'setup-stream-indirection
          (append *standard-output-streams*
                  *standard-input-streams*
                  *standard-io-streams*))))

(add-hook *after-init-hook* 'init-global-stream-redirection)

(defun globally-redirect-io-to-connection (connection)
  "Set the standard I/O streams to redirect to CONNECTION.
Assigns *CURRENT-<STREAM>* for all standard streams."
  (dolist (o *standard-output-streams*)
    (set (prefixed-var '#:current o)
         (connection.user-output connection)))
  ;; FIXME: If we redirect standard input to Emacs then we get the
  ;; regular Lisp top-level trying to read from our REPL.
  ;;
  ;; Perhaps the ideal would be for the real top-level to run in a
  ;; thread with local bindings for all the standard streams. Failing
  ;; that we probably would like to inhibit it from reading while
  ;; Emacs is connected.
  ;;
  ;; Meanwhile we just leave *standard-input* alone.
  #+NIL
  (dolist (i *standard-input-streams*)
    (set (prefixed-var '#:current i)
         (connection.user-input connection)))
  (dolist (io *standard-io-streams*)
    (set (prefixed-var '#:current io)
         (connection.user-io connection))))

(defun revert-global-io-redirection ()
  "Set *CURRENT-<STREAM>* to *REAL-<STREAM>* for all standard streams."
  (dolist (stream-var (append *standard-output-streams*
                              *standard-input-streams*
                              *standard-io-streams*))
    (set (prefixed-var '#:current stream-var)
         (getf *saved-global-streams* stream-var))))

;;;;; Global redirection hooks

(defvar *global-stdio-connection* nil
  "The connection to which standard I/O streams are globally redirected.
NIL if streams are not globally redirected.")

(defun maybe-redirect-global-io (connection)
  "Consider globally redirecting to a newly-established CONNECTION."
  (when (and *globally-redirect-io* (null *global-stdio-connection*))
    (setq *global-stdio-connection* connection)
    (globally-redirect-io-to-connection connection)))

(defun update-redirection-after-close (closed-connection)
  "Update redirection after a connection closes."
  (check-type closed-connection connection)
  (when (eq *global-stdio-connection* closed-connection)
    (if (and (default-connection) *globally-redirect-io*)
        ;; Redirect to another connection.
        (globally-redirect-io-to-connection (default-connection))
        ;; No more connections, revert to the real streams.
        (progn (revert-global-io-redirection)
               (setq *global-stdio-connection* nil)))))

(add-hook *new-connection-hook*    'maybe-redirect-global-io)
(add-hook *connection-closed-hook* 'update-redirection-after-close)

;;;;; Redirection during requests
;;;
;;; We always redirect the standard streams to Emacs while evaluating
;;; an RPC. This is done with simple dynamic bindings.

(defun call-with-redirected-io (connection function)
  "Call FUNCTION with I/O streams redirected via CONNECTION."
  (declare (type function function))
  (let* ((io  (connection.user-io connection))
         (in  (connection.user-input connection))
         (out (connection.user-output connection))
         (trace (or (connection.trace-output connection) out))
         (*standard-output* out)
         (*error-output* out)
         (*trace-output* trace)
         (*debug-io* io)
         (*query-io* io)
         (*standard-input* in)
         (*terminal-io* io))
    (funcall function)))

(defun call-with-thread-description (description thunk)
  ;; For `M-x slime-list-threads': Display what threads
  ;; created by swank are currently doing.
  (flet ((request-to-string (req)
           (remove #\Newline
                   (string-trim '(#\Space #\Tab)
                                (prin1-to-string req))))
         (truncate-string (str n)
           (format nil "~A..." (subseq str 0 (min (length str) n)))))
    (let* ((thread (current-thread))
           (old-description (thread-description thread)))
      (set-thread-description thread 
                              (truncate-string (request-to-string description)
                                               55))
      (unwind-protect (funcall thunk)
        (set-thread-description thread old-description)))))

(defmacro with-thread-description (description &body body)
  `(call-with-thread-description ,description #'(lambda () ,@body)))

(defun decode-message (stream)
  "Read an S-expression from STREAM using the SLIME protocol."
  ;;(log-event "decode-message~%")
  (let ((*swank-state-stack* (cons :read-next-form *swank-state-stack*)))
    (handler-bind ((error (lambda (c) (error (make-swank-error c)))))
      (let ((packet (read-packet stream)))
        (handler-case (values (read-form packet) nil)
          (reader-error (c) 
            `(:reader-error ,packet ,c)))))))

;; use peek-char to detect EOF, read-sequence may return 0 instead of
;; signaling a condition.
(defun read-packet (stream)
  (peek-char nil stream) 
  (let* ((header (read-chunk stream 6))
         (length (parse-integer header :radix #x10))
         (payload (read-chunk stream length)))
    (log-event "READ: ~S~%" payload)
    payload))

(defun read-chunk (stream length)
  (let* ((buffer (make-string length))
         (count (read-sequence buffer stream)))
    (assert (= count length) () "Short read: length=~D  count=~D" length count)
    buffer))

(defun read-form (string)
  (with-standard-io-syntax
    (let ((*package* *swank-io-package*))
      (read-from-string string))))

(defun input-available-p (stream)
  ;; return true iff we can read from STREAM without waiting or if we
  ;; hit EOF
  (let ((c (read-char-no-hang stream nil :eof)))
    (cond ((not c) nil)
          ((eq c :eof) t)
          (t 
           (unread-char c stream)
           t))))

(defvar *slime-features* nil
  "The feature list that has been sent to Emacs.")

(defun send-oob-to-emacs (object)
  (send-to-emacs object))

(defun encode-message (message stream)
  (let* ((string (prin1-to-string-for-emacs message))
         (length (length string)))
    (assert (<= length #xffffff))
    (log-event "WRITE: ~A~%" string)
    (let ((*print-pretty* nil))
      (format stream "~6,'0x" length))
    (write-string string stream)
    ;;(terpri stream)
    (finish-output stream)))

(defun prin1-to-string-for-emacs (object)
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-readably* nil)
          (*print-pretty* nil)
          (*package* *swank-io-package*))
      (prin1-to-string object))))

(defun force-user-output ()
  (force-output (connection.user-io *emacs-connection*)))

(add-hook *pre-reply-hook* 'force-user-output)

(defun clear-user-input  ()
  (clear-input (connection.user-input *emacs-connection*)))

(defvar *tag-counter* 0)

(defun make-tag () 
  (setq *tag-counter* (mod (1+ *tag-counter*) (expt 2 22))))

(defun read-user-input-from-emacs ()
  (let ((tag (make-tag)))
    (force-output)
    (send-to-emacs `(:read-string ,(current-thread-id) ,tag))
    (let ((ok nil))
      (unwind-protect
           (prog1 (caddr (wait-for-event `(:emacs-return-string ,tag value)))
             (setq ok t))
        (unless ok 
          (send-to-emacs `(:read-aborted ,(current-thread-id) ,tag)))))))

(defun y-or-n-p-in-emacs (format-string &rest arguments)
  "Like y-or-n-p, but ask in the Emacs minibuffer."
  (let ((tag (make-tag))
        (question (apply #'format nil format-string arguments)))
    (force-output)
    (send-to-emacs `(:y-or-n-p ,(current-thread-id) ,tag ,question))
    (caddr (wait-for-event `(:emacs-return ,tag result)))))

(defun process-form-for-emacs (form)
  "Returns a string which emacs will read as equivalent to
FORM. FORM can contain lists, strings, characters, symbols and
numbers.

Characters are converted emacs' ?<char> notaion, strings are left
as they are (except for espacing any nested \" chars, numbers are
printed in base 10 and symbols are printed as their symbol-name
converted to lower case."
  (etypecase form
    (string (format nil "~S" form))
    (cons (format nil "(~A . ~A)"
                  (process-form-for-emacs (car form))
                  (process-form-for-emacs (cdr form))))
    (character (format nil "?~C" form))
    (symbol (concatenate 'string (when (eq (symbol-package form)
                                           #.(find-package "KEYWORD"))
                                   ":")
                         (string-downcase (symbol-name form))))
    (number (let ((*print-base* 10))
              (princ-to-string form)))))

(defun eval-in-emacs (form &optional nowait)
  "Eval FORM in Emacs."
  (cond (nowait 
         (send-to-emacs `(:eval-no-wait ,(process-form-for-emacs form))))
        (t
         (force-output)
         (let ((tag (make-tag)))
	   (send-to-emacs `(:eval ,(current-thread-id) ,tag 
				  ,(process-form-for-emacs form)))
	   (let ((value (caddr (wait-for-event `(:emacs-return ,tag result)))))
	     (destructure-case value
	       ((:ok value) value)
	       ((:abort) (abort))))))))

(defvar *swank-wire-protocol-version* nil
  "The version of the swank/slime communication protocol.")

(defslimefun connection-info ()
  "Return a key-value list of the form: 
\(&key PID STYLE LISP-IMPLEMENTATION MACHINE FEATURES PACKAGE VERSION)
PID: is the process-id of Lisp process (or nil, depending on the STYLE)
STYLE: the communication style
LISP-IMPLEMENTATION: a list (&key TYPE NAME VERSION)
FEATURES: a list of keywords
PACKAGE: a list (&key NAME PROMPT)
VERSION: the protocol version"
  (setq *slime-features* *features*)
  `(:pid ,(getpid) :style ,(connection.communication-style *emacs-connection*)
    :lisp-implementation (:type ,(lisp-implementation-type)
                          :name ,(lisp-implementation-type-name)
                          :version ,(lisp-implementation-version))
    :machine (:instance ,(machine-instance)
              :type ,(machine-type)
              :version ,(machine-version))
    :features ,(features-for-emacs)
    :modules ,*modules*
    :package (:name ,(package-name *package*)
              :prompt ,(package-string-for-prompt *package*))
    :version ,*swank-wire-protocol-version*))

(defslimefun io-speed-test (&optional (n 1000) (m 1))
  (let* ((s *standard-output*)
         (*trace-output* (make-broadcast-stream s *log-output*)))
    (time (progn
            (dotimes (i n)
              (format s "~D abcdefghijklm~%" i)
              (when (zerop (mod n m))
                (finish-output s)))
            (finish-output s)
            (when *emacs-connection*
              (eval-in-emacs '(message "done.")))))
    (terpri *trace-output*)
    (finish-output *trace-output*)
    nil))


;;;; Reading and printing

(defmacro define-special (name doc)
  "Define a special variable NAME with doc string DOC.
This is like defvar, but NAME will not be initialized."
  `(progn
    (defvar ,name)
    (setf (documentation ',name 'variable) ,doc)))

(define-special *buffer-package*     
    "Package corresponding to slime-buffer-package.  

EVAL-FOR-EMACS binds *buffer-package*.  Strings originating from a slime
buffer are best read in this package.  See also FROM-STRING and TO-STRING.")

(define-special *buffer-readtable*
    "Readtable associated with the current buffer")

(defmacro with-buffer-syntax ((&optional package) &body body)
  "Execute BODY with appropriate *package* and *readtable* bindings.

This should be used for code that is conceptionally executed in an
Emacs buffer."
  `(call-with-buffer-syntax ,package (lambda () ,@body)))

(defun call-with-buffer-syntax (package fun)
  (let ((*package* (if package 
                       (guess-buffer-package package) 
                       *buffer-package*)))
    ;; Don't shadow *readtable* unnecessarily because that prevents
    ;; the user from assigning to it.
    (if (eq *readtable* *buffer-readtable*)
        (call-with-syntax-hooks fun)
        (let ((*readtable* *buffer-readtable*))
          (call-with-syntax-hooks fun)))))

(defun to-string (object)
  "Write OBJECT in the *BUFFER-PACKAGE*.
The result may not be readable. Handles problems with PRINT-OBJECT methods
gracefully."
  (with-buffer-syntax ()
    (let ((*print-readably* nil))
      (handler-case
          (prin1-to-string object)
        (error ()
          (with-output-to-string (s)
            (print-unreadable-object (object s :type t :identity t)
              (princ "<<error printing object>>" s))))))))

(defun from-string (string)
  "Read string in the *BUFFER-PACKAGE*"
  (with-buffer-syntax ()
    (let ((*read-suppress* nil))
      (read-from-string string))))

(defun parse-string (string package)
  "Read STRING in PACKAGE."
  (with-buffer-syntax (package)
    (let ((*read-suppress* nil))
      (read-from-string string))))

;; FIXME: deal with #\| etc.  hard to do portably.
(defun tokenize-symbol (string)
  "STRING is interpreted as the string representation of a symbol
and is tokenized accordingly. The result is returned in three
values: The package identifier part, the actual symbol identifier
part, and a flag if the STRING represents a symbol that is
internal to the package identifier part. (Notice that the flag is
also true with an empty package identifier part, as the STRING is
considered to represent a symbol internal to some current package.)"
  (let ((package (let ((pos (position #\: string)))
                   (if pos (subseq string 0 pos) nil)))
        (symbol (let ((pos (position #\: string :from-end t)))
                  (if pos (subseq string (1+ pos)) string)))
        (internp (not (= (count #\: string) 1))))
    (values symbol package internp)))

(defun tokenize-symbol-thoroughly (string)
  "This version of TOKENIZE-SYMBOL handles escape characters."
  (let ((package nil)
        (token (make-array (length string) :element-type 'character
                           :fill-pointer 0))
        (backslash nil)
        (vertical nil)
        (internp nil))
    (loop for char across string
       do (cond
            (backslash
             (vector-push-extend char token)
             (setq backslash nil))
            ((char= char #\\) ; Quotes next character, even within |...|
             (setq backslash t))
            ((char= char #\|)
             (setq vertical t))
            (vertical
             (vector-push-extend char token))
            ((char= char #\:)
             (if package
                 (setq internp t)
                 (setq package token
                       token (make-array (length string)
                                         :element-type 'character
                                         :fill-pointer 0))))
            (t
             (vector-push-extend (casify-char char) token))))
    (values token package (or (not package) internp))))

(defun untokenize-symbol (package-name internal-p symbol-name)
  "The inverse of TOKENIZE-SYMBOL.

  (untokenize-symbol \"quux\" nil \"foo\") ==> \"quux:foo\"
  (untokenize-symbol \"quux\" t \"foo\")   ==> \"quux::foo\"
  (untokenize-symbol nil nil \"foo\")    ==> \"foo\"
"
  (cond ((not package-name) 	symbol-name)
        (internal-p 		(cat package-name "::" symbol-name))
        (t 			(cat package-name ":" symbol-name))))

(defun casify-char (char)
  "Convert CHAR accoring to readtable-case."
  (ecase (readtable-case *readtable*)
    (:preserve char)
    (:upcase   (char-upcase char))
    (:downcase (char-downcase char))
    (:invert (if (upper-case-p char)
                 (char-downcase char)
                 (char-upcase char)))))

(defun parse-symbol (string &optional (package *package*))
  "Find the symbol named STRING.
Return the symbol and a flag indicating whether the symbols was found."
  (multiple-value-bind (sname pname) (tokenize-symbol-thoroughly string)
    (let ((package (cond ((string= pname "") keyword-package)
                         (pname              (find-package pname))
                         (t                  package))))
      (if package
          (multiple-value-bind (symbol flag) (find-symbol sname package)
            (values symbol flag sname package))
          (values nil nil nil nil)))))

(defun parse-symbol-or-lose (string &optional (package *package*))
  (multiple-value-bind (symbol status) (parse-symbol string package)
    (if status
        (values symbol status)
        (error "Unknown symbol: ~A [in ~A]" string package))))

(defun parse-package (string)
  "Find the package named STRING.
Return the package or nil."
  ;; STRING comes usually from a (in-package STRING) form.
  (ignore-errors
    (find-package (let ((*package* *swank-io-package*))
                    (read-from-string string)))))

(defun unparse-name (string)
  "Print the name STRING according to the current printer settings."
  ;; this is intended for package or symbol names
  (subseq (prin1-to-string (make-symbol string)) 2))

(defun guess-package (string)
  "Guess which package corresponds to STRING.
Return nil if no package matches."
  (when string
    (or (find-package string)
        (parse-package string)
        (if (find #\! string)           ; for SBCL
            (guess-package (substitute #\- #\! string))))))

(defvar *readtable-alist* (default-readtable-alist)
  "An alist mapping package names to readtables.")

(defun guess-buffer-readtable (package-name)
  (let ((package (guess-package package-name)))
    (or (and package 
             (cdr (assoc (package-name package) *readtable-alist* 
                         :test #'string=)))
        *readtable*)))


;;;; Evaluation

(defvar *pending-continuations* '()
  "List of continuations for Emacs. (thread local)")

(defun guess-buffer-package (string)
  "Return a package for STRING. 
Fall back to the the current if no such package exists."
  (or (and string (guess-package string))
      *package*))

(defun eval-for-emacs (form buffer-package id)
  "Bind *BUFFER-PACKAGE* to BUFFER-PACKAGE and evaluate FORM.
Return the result to the continuation ID.
Errors are trapped and invoke our debugger."
  (let (ok result)
    (unwind-protect
         (let ((*buffer-package* (guess-buffer-package buffer-package))
               (*buffer-readtable* (guess-buffer-readtable buffer-package))
               (*pending-continuations* (cons id *pending-continuations*)))
           (check-type *buffer-package* package)
           (check-type *buffer-readtable* readtable)
           ;; APPLY would be cleaner than EVAL. 
           ;;(setq result (apply (car form) (cdr form)))
           (setq result (with-slime-interrupts (eval form)))
           (run-hook *pre-reply-hook*)
           (setq ok t))
      (send-to-emacs `(:return ,(current-thread)
                               ,(if ok
                                    `(:ok ,result)
                                    `(:abort))
                               ,id)))))

(defvar *echo-area-prefix* "=> "
  "A prefix that `format-values-for-echo-area' should use.")

(defun format-values-for-echo-area (values)
  (with-buffer-syntax ()
    (let ((*print-readably* nil))
      (cond ((null values) "; No value")
            ((and (integerp (car values)) (null (cdr values)))
             (let ((i (car values)))
               (format nil "~A~D (#x~X, #o~O, #b~B)" 
                       *echo-area-prefix* i i i i)))
            (t (format nil "~a~{~S~^, ~}" *echo-area-prefix* values))))))

(defslimefun interactive-eval (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLIME interactive evaluation request.")
      (let ((values (multiple-value-list (eval (from-string string)))))
        (finish-output)
        (format-values-for-echo-area values)))))

(defslimefun eval-and-grab-output (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLIME evaluation request.")
      (let* ((s (make-string-output-stream))
             (*standard-output* s)
             (values (multiple-value-list (eval (from-string string)))))
        (list (get-output-stream-string s) 
              (format nil "~{~S~^~%~}" values))))))

(defun eval-region (string)
  "Evaluate STRING.
Return the results of the last form as a list and as secondary value the 
last form."
  (with-input-from-string (stream string)
    (let (- values)
      (loop
       (let ((form (read stream nil stream)))
         (when (eq form stream)
           (return (values values -)))
         (setq - form)
         (setq values (multiple-value-list (eval form)))
         (finish-output))))))

(defslimefun interactive-eval-region (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLIME interactive evaluation request.")
      (format-values-for-echo-area (eval-region string)))))

(defslimefun re-evaluate-defvar (form)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLIME evaluation request.")
      (let ((form (read-from-string form)))
        (destructuring-bind (dv name &optional value doc) form
          (declare (ignore value doc))
          (assert (eq dv 'defvar))
          (makunbound name)
          (prin1-to-string (eval form)))))))

(defvar *swank-pprint-bindings*
  `((*print-pretty*   . t) 
    (*print-level*    . nil)
    (*print-length*   . nil)
    (*print-circle*   . t)
    (*print-gensym*   . t)
    (*print-readably* . nil))
  "A list of variables bindings during pretty printing.
Used by pprint-eval.")

(defun swank-pprint (list)
  "Bind some printer variables and pretty print each object in LIST."
  (with-buffer-syntax ()
    (with-bindings *swank-pprint-bindings*
      (cond ((null list) "; No value")
            (t (with-output-to-string (*standard-output*)
                 (dolist (o list)
                   (pprint o)
                   (terpri))))))))
  
(defslimefun pprint-eval (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLIME evaluation request.")
      (swank-pprint (multiple-value-list (eval (read-from-string string)))))))

(defslimefun set-package (name)
  "Set *package* to the package named NAME.
Return the full package-name and the string to use in the prompt."
  (let ((p (guess-package name)))
    (assert (packagep p))
    (setq *package* p)
    (list (package-name p) (package-string-for-prompt p))))

;;;;; Listener eval

(defvar *listener-eval-function* 'repl-eval)

(defslimefun listener-eval (string)
  (funcall *listener-eval-function* string))

(defvar *send-repl-results-function* 'send-repl-results-to-emacs)

(defun repl-eval (string)
  (clear-user-input)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLIME REPL evaluation request.")
      (track-package 
       (lambda ()
         (multiple-value-bind (values last-form) (eval-region string)
           (setq *** **  ** *  * (car values)
                 /// //  // /  / values
                 +++ ++  ++ +  + last-form)
           (funcall *send-repl-results-function* values))))))
  nil)

(defun track-package (fun)
  (let ((p *package*))
    (unwind-protect (funcall fun)
      (unless (eq *package* p)
        (send-to-emacs (list :new-package (package-name *package*)
                             (package-string-for-prompt *package*)))))))

(defun send-repl-results-to-emacs (values)    
  (finish-output)
  (if (null values)
      (send-to-emacs `(:write-string "; No value" :repl-result))
      (dolist (v values)
        (send-to-emacs `(:write-string ,(cat (prin1-to-string v) #\newline)
                                       :repl-result)))))

(defun cat (&rest strings)
  "Concatenate all arguments and make the result a string."
  (with-output-to-string (out)
    (dolist (s strings)
      (etypecase s
        (string (write-string s out))
        (character (write-char s out))))))

(defun truncate-string (string width &optional ellipsis)
  (let ((len (length string)))
    (cond ((< len width) string)
          (ellipsis (cat (subseq string 0 width) ellipsis))
          (t (subseq string 0 width)))))

(defun call/truncated-output-to-string (length function 
                                        &optional (ellipsis ".."))
  "Call FUNCTION with a new stream, return the output written to the stream.
If FUNCTION tries to write more than LENGTH characters, it will be
aborted and return immediately with the output written so far."
  (let ((buffer (make-string (+ length (length ellipsis))))
        (fill-pointer 0))
    (block buffer-full
      (flet ((write-output (string)
               (let* ((free (- length fill-pointer))
                      (count (min free (length string))))
                 (replace buffer string :start1 fill-pointer :end2 count)
                 (incf fill-pointer count)
                 (when (> (length string) free)
                   (replace buffer ellipsis :start1 fill-pointer)
                   (return-from buffer-full buffer)))))
        (let ((stream (make-output-stream #'write-output)))
          (funcall function stream)
          (finish-output stream)
          (subseq buffer 0 fill-pointer))))))

(defun package-string-for-prompt (package)
  "Return the shortest nickname (or canonical name) of PACKAGE."
  (unparse-name
   (or (canonical-package-nickname package)
       (auto-abbreviated-package-name package)
       (shortest-package-nickname package))))

(defun canonical-package-nickname (package)
  "Return the canonical package nickname, if any, of PACKAGE."
  (let ((name (cdr (assoc (package-name package) *canonical-package-nicknames* 
                          :test #'string=))))
    (and name (string name))))

(defun auto-abbreviated-package-name (package)
  "Return an abbreviated 'name' for PACKAGE. 

N.B. this is not an actual package name or nickname."
  (when *auto-abbreviate-dotted-packages*
    (loop with package-name = (package-name package)
          with offset = nil
          do (let ((last-dot-pos (position #\. package-name :end offset :from-end t)))
               (unless last-dot-pos
                 (return nil))
               ;; If a dot chunk contains only numbers, that chunk most
               ;; likely represents a version number; so we collect the
               ;; next chunks, too, until we find one with meat.
               (let ((name (subseq package-name (1+ last-dot-pos) offset)))
                 (if (notevery #'digit-char-p name)
                     (return (subseq package-name (1+ last-dot-pos)))
                     (setq offset last-dot-pos)))))))

(defun shortest-package-nickname (package)
  "Return the shortest nickname of PACKAGE."
  (loop for name in (cons (package-name package) (package-nicknames package))
        for shortest = name then (if (< (length name) (length shortest))
                                   name
                                   shortest)
              finally (return shortest)))

(defslimefun ed-in-emacs (&optional what)
  "Edit WHAT in Emacs.

WHAT can be:
  A pathname or a string,
  A list (PATHNAME-OR-STRING LINE [COLUMN]),
  A function name (symbol or cons),
  NIL.

Returns true if it actually called emacs, or NIL if not."
  (flet ((pathname-or-string-p (thing)
           (or (pathnamep thing) (typep thing 'string)))
         (canonicalize-filename (filename)
           (let ((file-name (or (probe-file filename) filename)))
             #-scl (namestring file-name)
             #+scl (ext:unix-namestring file-name nil))))
    (let ((target
           (cond ((and (listp what) (pathname-or-string-p (first what)))
                  (cons (canonicalize-filename (car what)) (cdr what)))
                 ((pathname-or-string-p what)
                  (canonicalize-filename what))
                 ((symbolp what) what)
                 ((consp what) what)
                 (t (return-from ed-in-emacs nil)))))
      (cond
        (*emacs-connection* (send-oob-to-emacs `(:ed ,target)))
        ((default-connection)
         (with-connection ((default-connection))
           (send-oob-to-emacs `(:ed ,target))))
        (t nil)))))

(defslimefun inspect-in-emacs (what)
  "Inspect WHAT in Emacs."
  (flet ((send-it ()
           (with-buffer-syntax ()
             (reset-inspector)
             (send-oob-to-emacs `(:inspect ,(inspect-object what))))))
    (cond 
      (*emacs-connection*
       (send-it))
      ((default-connection)
       (with-connection ((default-connection))
         (send-it))))
    what))

(defslimefun value-for-editing (form)
  "Return a readable value of FORM for editing in Emacs.
FORM is expected, but not required, to be SETF'able."
  ;; FIXME: Can we check FORM for setfability? -luke (12/Mar/2005)
  (with-buffer-syntax ()
    (prin1-to-string (eval (read-from-string form)))))

(defslimefun commit-edited-value (form value)
  "Set the value of a setf'able FORM to VALUE.
FORM and VALUE are both strings from Emacs."
  (with-buffer-syntax ()
    (eval `(setf ,(read-from-string form) 
            ,(read-from-string (concatenate 'string "`" value))))
    t))

(defun background-message  (format-string &rest args)
  "Display a message in Emacs' echo area.

Use this function for informative messages only.  The message may even
be dropped, if we are too busy with other things."
  (when *emacs-connection*
    (send-to-emacs `(:background-message 
                     ,(apply #'format nil format-string args)))))


;;;; Debugger

(defun invoke-slime-debugger (condition)
  "Sends a message to Emacs declaring that the debugger has been entered,
then waits to handle further requests from Emacs. Eventually returns
after Emacs causes a restart to be invoked."
  (without-slime-interrupts
    (cond (*emacs-connection*
           (debug-in-emacs condition))
          ((default-connection)
           (with-connection ((default-connection))
             (debug-in-emacs condition))))))

(defun swank-debugger-hook (condition hook)
  "Debugger function for binding *DEBUGGER-HOOK*."
  (declare (ignore hook))
  (call-with-debugger-hook #'swank-debugger-hook
                           (lambda () (invoke-slime-debugger condition))))

(defun invoke-default-debugger (condition)
  (let ((*debugger-hook* nil))
    (invoke-debugger condition)))
  
(defvar *global-debugger* t
  "Non-nil means the Swank debugger hook will be installed globally.")

(add-hook *new-connection-hook* 'install-debugger)
(defun install-debugger (connection)
  (declare (ignore connection))
  (when *global-debugger*
    (install-debugger-globally #'swank-debugger-hook)))

;;;;; Debugger loop
;;;
;;; These variables are dynamically bound during debugging.
;;;
(defvar *swank-debugger-condition* nil
  "The condition being debugged.")

(defvar *sldb-level* 0
  "The current level of recursive debugging.")

(defvar *sldb-initial-frames* 20
  "The initial number of backtrace frames to send to Emacs.")

(defvar *sldb-restarts* nil
  "The list of currenlty active restarts.")

(defvar *sldb-stepping-p* nil
  "True during execution of a step command.")

(defun debug-in-emacs (condition)
  (let ((*swank-debugger-condition* condition)
        (*sldb-restarts* (compute-sane-restarts condition))
        (*package* (or (and (boundp '*buffer-package*)
                            (symbol-value '*buffer-package*))
                       *package*))
        (*sldb-level* (1+ *sldb-level*))
        (*sldb-stepping-p* nil)
        (*swank-state-stack* (cons :swank-debugger-hook *swank-state-stack*)))
    (force-user-output)
    (call-with-debugging-environment
     (lambda ()
       (with-bindings *sldb-printer-bindings*
         (sldb-loop *sldb-level*))))))

(defun sldb-loop (level)
  (unwind-protect
       (loop
        (with-simple-restart (abort "Return to sldb level ~D." level)
          (send-to-emacs 
           (list* :debug (current-thread-id) level
                  (debugger-info-for-emacs 0 *sldb-initial-frames*)))
          (send-to-emacs 
           (list :debug-activate (current-thread-id) level nil))
          (loop 
           (handler-case 
               (destructure-case (wait-for-event 
                                  `(or (:emacs-rex . _)
                                       (:sldb-return ,(1+ level))))
                 ((:emacs-rex &rest args) (apply #'eval-for-emacs args))
                 ((:sldb-return _) (declare (ignore _)) (return nil))) 
             (sldb-condition (c) 
               (handle-sldb-condition c))))))
    (send-to-emacs `(:debug-return
                     ,(current-thread-id) ,level ,*sldb-stepping-p*))
    (wait-for-event `(:sldb-return ,(1+ level)) t) ; clean event-queue
    (when (> level 1)
      (send-event (current-thread) `(:sldb-return ,level)))))

(defun handle-sldb-condition (condition)
  "Handle an internal debugger condition.
Rather than recursively debug the debugger (a dangerous idea!), these
conditions are simply reported."
  (let ((real-condition (original-condition condition)))
    (send-to-emacs `(:debug-condition ,(current-thread-id)
                                      ,(princ-to-string real-condition)))))

(defvar *sldb-condition-printer* #'format-sldb-condition
  "Function called to print a condition to an SLDB buffer.")

(defun safe-condition-message (condition)
  "Safely print condition to a string, handling any errors during
printing."
  (let ((*print-pretty* t) (*print-right-margin* 65))
    (handler-case
        (funcall *sldb-condition-printer* condition)
      (error (cond)
        ;; Beware of recursive errors in printing, so only use the condition
        ;; if it is printable itself:
        (format nil "Unable to display error condition~@[: ~A~]"
                (ignore-errors (princ-to-string cond)))))))

(defun debugger-condition-for-emacs ()
  (list (safe-condition-message *swank-debugger-condition*)
        (format nil "   [Condition of type ~S]"
                (type-of *swank-debugger-condition*))
        (condition-extras *swank-debugger-condition*)))

(defun format-restarts-for-emacs ()
  "Return a list of restarts for *swank-debugger-condition* in a
format suitable for Emacs."
  (let ((*print-right-margin* most-positive-fixnum))
    (loop for restart in *sldb-restarts*
          collect (list (princ-to-string (restart-name restart))
                        (princ-to-string restart)))))


;;;;; SLDB entry points

(defslimefun sldb-break-with-default-debugger ()
  "Invoke the default debugger."
  (call-with-debugger-hook
   nil (lambda () (invoke-debugger *swank-debugger-condition*)))
  (send-to-emacs 
   (list :debug-activate (current-thread-id) *sldb-level* t)))

(defslimefun backtrace (start end)
  "Return a list ((I FRAME PLIST) ...) of frames from START to END.

I is an integer, and can be used to reference the corresponding frame
from Emacs; FRAME is a string representation of an implementation's
frame."
  (flet ((print-swank-frame-to-string (frame)
           (call/truncated-output-to-string 
            100
            (lambda (stream)
              (handler-case
                  (with-bindings *backtrace-printer-bindings*
                    (print-swank-frame frame stream))
                (t ()
                  (format stream "[error printing frame]")))))))
    (loop for frame in (compute-backtrace start end)
          for i from start collect 
          (list i (print-swank-frame-to-string frame)
                (list :restartable (let ((r (swank-frame.restartable frame)))
                                     (check-type r (member nil t :unknown))
                                     r))))))

(defslimefun debugger-info-for-emacs (start end)
  "Return debugger state, with stack frames from START to END.
The result is a list:
  (condition ({restart}*) ({stack-frame}*) (cont*))
where
  condition   ::= (description type [extra])
  restart     ::= (name description)
  stack-frame ::= (number description [plist])
  extra       ::= (:references and other random things)
  cont        ::= continutation
  plist       ::= (:restartable {nil | t | :unknown})

condition---a pair of strings: message, and type.  If show-source is
not nil it is a frame number for which the source should be displayed.

restart---a pair of strings: restart name, and description.

stack-frame---a number from zero (the top), and a printed
representation of the frame's call.

continutation---the id of a pending Emacs continuation.

Below is an example return value. In this case the condition was a
division by zero (multi-line description), and only one frame is being
fetched (start=0, end=1).

 ((\"Arithmetic error DIVISION-BY-ZERO signalled.
Operation was KERNEL::DIVISION, operands (1 0).\"
   \"[Condition of type DIVISION-BY-ZERO]\")
  ((\"ABORT\" \"Return to Slime toplevel.\")
   (\"ABORT\" \"Return to Top-Level.\"))
  ((0 \"(KERNEL::INTEGER-/-INTEGER 1 0)\" (:restartable nil)))
  (4))"
  (list (debugger-condition-for-emacs)
        (format-restarts-for-emacs)
        (backtrace start end)
        *pending-continuations*))

(defun nth-restart (index)
  (nth index *sldb-restarts*))

(defslimefun invoke-nth-restart (index)
  (invoke-restart-interactively (nth-restart index)))

(defslimefun sldb-abort ()
  (invoke-restart (find 'abort *sldb-restarts* :key #'restart-name)))

(defslimefun sldb-continue ()
  (continue))

(defslimefun throw-to-toplevel ()
  "Invoke the ABORT-REQUEST restart abort an RPC from Emacs.
If we are not evaluating an RPC then ABORT instead."
  (let ((restart (and (not (symbolp *sldb-quit-restart*))
                      (find-restart *sldb-quit-restart*))))
    (cond (restart (invoke-restart restart))
          (t (format nil
                     "Restart not found: ~a"
                     *sldb-quit-restart*)))))

(defslimefun invoke-nth-restart-for-emacs (sldb-level n)
  "Invoke the Nth available restart.
SLDB-LEVEL is the debug level when the request was made. If this
has changed, ignore the request."
  (when (= sldb-level *sldb-level*)
    (invoke-nth-restart n)))

(defun wrap-sldb-vars (form)
  `(let ((*sldb-level* ,*sldb-level*))
     ,form))

(defslimefun eval-string-in-frame (string index)
  (to-string
   (with-retry-restart (:msg "Retry SLIME evaluation request.")
     (eval-in-frame (wrap-sldb-vars (from-string string))
                    index))))

(defslimefun pprint-eval-string-in-frame (string index)
  (swank-pprint
   (with-retry-restart (:msg "Retry SLIME evaluation request.")
     (multiple-value-list 
      (eval-in-frame (wrap-sldb-vars (from-string string)) index)))))

(defslimefun frame-locals-for-emacs (index)
  "Return a property list ((&key NAME ID VALUE) ...) describing
the local variables in the frame INDEX."
  (with-bindings *backtrace-printer-bindings*
    (mapcar (lambda (frame-locals)
              (destructuring-bind (&key name id value) frame-locals
                (list :name (prin1-to-string name) :id id
                      :value (to-line value))))
            (frame-locals index))))

(defslimefun frame-catch-tags-for-emacs (frame-index)
  (mapcar #'to-string (frame-catch-tags frame-index)))

(defslimefun sldb-disassemble (index)
  (with-output-to-string (*standard-output*)
    (disassemble-frame index)))

(defslimefun sldb-return-from-frame (index string)
  (let ((form (from-string string)))
    (to-string (multiple-value-list (return-from-frame index form)))))

(defslimefun sldb-break (name)
  (with-buffer-syntax ()
    (sldb-break-at-start (read-from-string name))))

(defmacro define-stepper-function (name backend-function-name)
  `(defslimefun ,name (frame)
     (cond ((sldb-stepper-condition-p *swank-debugger-condition*)
            (setq *sldb-stepping-p* t)
            (,backend-function-name))
           ((find-restart 'continue)
         (activate-stepping frame)
         (setq *sldb-stepping-p* t)
         (continue))
        (t
            (error "Not currently single-stepping, and no continue restart available.")))))

(define-stepper-function sldb-step sldb-step-into)
(define-stepper-function sldb-next sldb-step-next)
(define-stepper-function sldb-out  sldb-step-out)


;;;; Compilation Commands.

(defstruct (:swank-compilation-result
             (:type list) :named
             (:conc-name swank-compilation-result.)
             (:constructor make-swank-compilation-result ()))
  notes      ; 
  results    ; one result is of type (MEMBER T NIL :COMPLAINED)
  durations  ;
  )

(defun measure-time-interval (fn)
  "Call FN and return the first return value and the elapsed time.
The time is measured in microseconds."
  (declare (type function fn))
  (let ((before (get-internal-real-time)))
    (values
     (funcall fn)
     (* (- (get-internal-real-time) before)
        (/ 1000000 internal-time-units-per-second)))))

(defun make-compiler-note (condition)
  "Make a compiler note data structure from a compiler-condition."
  (declare (type compiler-condition condition))
  (list* :message (message condition)
         :severity (severity condition)
         :location (location condition)
         :references (references condition)
         (let ((s (short-message condition)))
           (if s (list :short-message s)))))

(defun swank-compilation-result-for-emacs (old)
  "Make a Swank-Compilation-Unit suitable for Emacs."
  (let ((new (make-swank-compilation-result)))
    (with-struct (swank-compilation-result. notes results durations) old
      (setf (swank-compilation-result.notes new)   (reverse notes))
      (setf (swank-compilation-result.results new) (reverse results))
      (setf (swank-compilation-result.durations new)
            (reverse (mapcar #'(lambda (usecs) (/ usecs 1000000.0)) durations))))
    new))

(defun swank-compiler (swank-compilation-result function)
  (let ((swank-result swank-compilation-result))
    (multiple-value-bind (result usecs)
        (with-simple-restart (abort-compilation "Abort SLIME compilation request.")
          (handler-bind ((compiler-condition
                          #'(lambda (c)
                              (push (make-compiler-note c)
                                    (swank-compilation-result.notes swank-result)))))
            (measure-time-interval function)))
      (when (eql usecs t) (setf usecs 0)) ; compilation aborted.      
      (when result
        (let ((notes-p (swank-compilation-result.notes swank-result)))
          (setf result (if notes-p :complained t))))
      (push result (swank-compilation-result.results swank-result))
      (push usecs  (swank-compilation-result.durations swank-result))
      swank-result)))

(defslimefun compile-file-for-emacs (filename load-p)
  "Compile FILENAME and, when LOAD-P, load the result.
Record compiler notes signalled as `compiler-condition's."
  (with-buffer-syntax ()
    (swank-compilation-result-for-emacs
     (swank-compiler (make-swank-compilation-result)
                     (lambda ()
                       (let ((pathname (parse-emacs-filename filename))
                             (*compile-print* nil) (*compile-verbose* t))
                         (swank-compile-file pathname load-p
                                             (or (guess-external-format pathname)
                                                 :default))))))))

(defslimefun compile-string-for-emacs (string buffer position directory debug)
  "Compile STRING (exerpted from BUFFER at POSITION).
Record compiler notes signalled as `compiler-condition's."
  (with-buffer-syntax ()
    (swank-compilation-result-for-emacs
     (swank-compiler (make-swank-compilation-result)
                     (lambda () 
                       (let ((*compile-print* t) (*compile-verbose* nil))
                         (swank-compile-string string
                                               :buffer buffer
                                               :position position 
                                               :directory directory
                                               :debug debug)))))))

(defslimefun compile-multiple-strings-for-emacs
    (strings buffers packages positions directories debug)
  "Compile STRING (exerpted from BUFFER at POSITION).
Record compiler notes signalled as `compiler-condition's."
  (let ((swank-compilation-result (make-swank-compilation-result)))
    (loop for string in strings
          for buffer in buffers
          for package in packages
          for position in positions
          for directory in directories do
          (swank-compiler swank-compilation-result
                          (lambda ()
                            (with-buffer-syntax (package)
                              (let ((*compile-print* t) (*compile-verbose* nil))
                                (swank-compile-string string
                                                      :buffer buffer
                                                      :position position 
                                                      :directory directory
                                                      :debug debug))))))
    (swank-compilation-result-for-emacs swank-compilation-result)))
  
(defun file-newer-p (new-file old-file)
  "Returns true if NEW-FILE is newer than OLD-FILE."
  (> (file-write-date new-file) (file-write-date old-file)))

(defun requires-compile-p (source-file)
  (let ((fasl-file (probe-file (compile-file-pathname source-file))))
    (or (not fasl-file)
        (file-newer-p source-file fasl-file))))

(defslimefun compile-file-if-needed (filename loadp)
  (let ((pathname (parse-emacs-filename filename)))
    (cond ((requires-compile-p pathname)
           (compile-file-for-emacs pathname loadp))
          (loadp
           (load (compile-file-pathname pathname))
           nil))))


;;;; Loading

(defslimefun load-file (filename)
  (to-string (load (parse-emacs-filename filename))))


;;;;; swank-require

(defslimefun swank-require (modules &optional filename)
  "Load the module MODULE."
  (dolist (module (if (listp modules) modules (list modules)))
    (unless (member (string module) *modules* :test #'string=)
      (require module (if filename
                          (parse-emacs-filename filename)
                          (module-filename module)))))
  *modules*)

(defvar *find-module* 'find-module
  "Pluggable function to locate modules.
The function receives a module name as argument and should return
the filename of the module (or nil if the file doesn't exist).")

(defun module-filename (module)
  "Return the filename for the module MODULE."
  (or (funcall *find-module* module)
      (error "Can't locate module: ~s" module)))

;;;;;; Simple *find-module* function.

(defun merged-directory (dirname defaults)
  (pathname-directory
   (merge-pathnames 
    (make-pathname :directory `(:relative ,dirname) :defaults defaults)
    defaults)))

(defvar *load-path* '()
  "A list of directories to search for modules.")

(defun module-canditates (name dir)
  (list (compile-file-pathname (make-pathname :name name :defaults dir))
        (make-pathname :name name :type "lisp" :defaults dir)))

(defun find-module (module)
  (let ((name (string-downcase module)))
    (some (lambda (dir) (some #'probe-file (module-canditates name dir)))
          *load-path*)))


;;;; Macroexpansion

(defvar *macroexpand-printer-bindings*
  '((*print-circle* . nil)
    (*print-pretty* . t)
    (*print-escape* . t)
    (*print-lines* . nil)
    (*print-level* . nil)
    (*print-length* . nil)))

(defun apply-macro-expander (expander string)
  (with-buffer-syntax ()
    (with-bindings *macroexpand-printer-bindings*
      (prin1-to-string (funcall expander (from-string string))))))

(defslimefun swank-macroexpand-1 (string)
  (apply-macro-expander #'macroexpand-1 string))

(defslimefun swank-macroexpand (string)
  (apply-macro-expander #'macroexpand string))

(defslimefun swank-macroexpand-all (string)
  (apply-macro-expander #'macroexpand-all string))

(defslimefun swank-compiler-macroexpand-1 (string)
  (apply-macro-expander #'compiler-macroexpand-1 string))

(defslimefun swank-compiler-macroexpand (string)
  (apply-macro-expander #'compiler-macroexpand string))

(defslimefun disassemble-symbol (name)
  (with-buffer-syntax ()
    (with-output-to-string (*standard-output*)
      (let ((*print-readably* nil))
        (disassemble (fdefinition (from-string name)))))))


;;;; Simple completion

(defslimefun simple-completions (string package)
  "Return a list of completions for the string STRING."
  (let ((strings (all-completions string package #'prefix-match-p)))
    (list strings (longest-common-prefix strings))))

(defun all-completions (string package test)
  (multiple-value-bind (name pname intern) (tokenize-symbol string)
    (let* ((extern (and pname (not intern)))
	   (pack (cond ((equal pname "") keyword-package)
		       ((not pname) (guess-buffer-package package))
		       (t (guess-package pname))))
	   (test (lambda (sym) (funcall test name (unparse-symbol sym))))
	   (syms (and pack (matching-symbols pack extern test))))
      (format-completion-set (mapcar #'unparse-symbol syms) intern pname))))

(defun matching-symbols (package external test)
  (let ((test (if external 
		  (lambda (s)
		    (and (symbol-external-p s package) 
			 (funcall test s)))
		  test))
	(result '()))
    (do-symbols (s package)
      (when (funcall test s) 
	(push s result)))
    (remove-duplicates result)))

(defun unparse-symbol (symbol)
  (let ((*print-case* (case (readtable-case *readtable*) 
                        (:downcase :upcase)
                        (t :downcase))))
    (unparse-name (symbol-name symbol))))

(defun prefix-match-p (prefix string)
  "Return true if PREFIX is a prefix of STRING."
  (not (mismatch prefix string :end2 (min (length string) (length prefix)))))

(defun longest-common-prefix (strings)
  "Return the longest string that is a common prefix of STRINGS."
  (if (null strings)
      ""
      (flet ((common-prefix (s1 s2)
               (let ((diff-pos (mismatch s1 s2)))
                 (if diff-pos (subseq s1 0 diff-pos) s1))))
        (reduce #'common-prefix strings))))

(defun format-completion-set (strings internal-p package-name)
  "Format a set of completion strings.
Returns a list of completions with package qualifiers if needed."
  (mapcar (lambda (string) (untokenize-symbol package-name internal-p string))
          (sort strings #'string<)))


;;;; Simple arglist display

(defslimefun operator-arglist (name package)
  (ignore-errors
    (let ((args (arglist (parse-symbol name (guess-buffer-package package))))
          (*print-escape* nil))
      (cond ((eq args :not-available) nil)
	    (t (format nil "(~a ~/pprint-fill/)" name args))))))


;;;; Documentation

(defslimefun apropos-list-for-emacs  (name &optional external-only 
                                           case-sensitive package)
  "Make an apropos search for Emacs.
The result is a list of property lists."
  (let ((package (if package
                     (or (parse-package package)
                         (error "No such package: ~S" package)))))
    ;; The MAPCAN will filter all uninteresting symbols, i.e. those
    ;; who cannot be meaningfully described.
    (mapcan (listify #'briefly-describe-symbol-for-emacs)
            (sort (remove-duplicates
                   (apropos-symbols name external-only case-sensitive package))
                  #'present-symbol-before-p))))

(defun briefly-describe-symbol-for-emacs (symbol)
  "Return a property list describing SYMBOL.
Like `describe-symbol-for-emacs' but with at most one line per item."
  (flet ((first-line (string)
           (let ((pos (position #\newline string)))
             (if (null pos) string (subseq string 0 pos)))))
    (let ((desc (map-if #'stringp #'first-line 
                        (describe-symbol-for-emacs symbol))))
      (if desc 
          (list* :designator (to-string symbol) desc)))))

(defun map-if (test fn &rest lists)
  "Like (mapcar FN . LISTS) but only call FN on objects satisfying TEST.
Example:
\(map-if #'oddp #'- '(1 2 3 4 5)) => (-1 2 -3 4 -5)"
  (apply #'mapcar
         (lambda (x) (if (funcall test x) (funcall fn x) x))
         lists))

(defun listify (f)
  "Return a function like F, but which returns any non-null value
wrapped in a list."
  (lambda (x)
    (let ((y (funcall f x)))
      (and y (list y)))))

(defun present-symbol-before-p (x y)
  "Return true if X belongs before Y in a printed summary of symbols.
Sorted alphabetically by package name and then symbol name, except
that symbols accessible in the current package go first."
  (declare (type symbol x y))
  (flet ((accessible (s)
           ;; Test breaks on NIL for package that does not inherit it
           (eq (find-symbol (symbol-name s) *buffer-package*) s)))
    (let ((ax (accessible x)) (ay (accessible y)))
      (cond ((and ax ay) (string< (symbol-name x) (symbol-name y)))
            (ax t)
            (ay nil)
            (t (let ((px (symbol-package x)) (py (symbol-package y)))
                 (if (eq px py)
                     (string< (symbol-name x) (symbol-name y))
                     (string< (package-name px) (package-name py)))))))))

(defun make-apropos-matcher (pattern case-sensitive)
  (let ((chr= (if case-sensitive #'char= #'char-equal)))
    (lambda (symbol)
      (search pattern (string symbol) :test chr=))))

(defun apropos-symbols (string external-only case-sensitive package)
  (let ((packages (or package (remove (find-package :keyword)
                                      (list-all-packages))))
        (matcher  (make-apropos-matcher string case-sensitive))
        (result))
    (with-package-iterator (next packages :external :internal)
      (loop (multiple-value-bind (morep symbol) (next)
              (cond ((not morep) (return))
                    ((and (if external-only (symbol-external-p symbol) t)
                          (funcall matcher symbol))
                     (push symbol result))))))
    result))

(defun call-with-describe-settings (fn)
  (let ((*print-readably* nil))
    (funcall fn)))

(defmacro with-describe-settings ((&rest _) &body body)
  (declare (ignore _))
  `(call-with-describe-settings (lambda () ,@body)))
    
(defun describe-to-string (object)
  (with-describe-settings ()
    (with-output-to-string (*standard-output*)
      (describe object))))

(defslimefun describe-symbol (symbol-name)
  (with-buffer-syntax ()
    (describe-to-string (parse-symbol-or-lose symbol-name))))

(defslimefun describe-function (name)
  (with-buffer-syntax ()
    (let ((symbol (parse-symbol-or-lose name)))
      (describe-to-string (or (macro-function symbol)
                              (symbol-function symbol))))))

(defslimefun describe-definition-for-emacs (name kind)
  (with-buffer-syntax ()
    (with-describe-settings ()
      (with-output-to-string (*standard-output*)
        (describe-definition (parse-symbol-or-lose name) kind)))))

(defslimefun documentation-symbol (symbol-name &optional default)
  (with-buffer-syntax ()
    (multiple-value-bind (sym foundp) (parse-symbol symbol-name)
      (if foundp
          (let ((vdoc (documentation sym 'variable))
                (fdoc (documentation sym 'function)))
            (or (and (or vdoc fdoc)
                     (concatenate 'string
                                  fdoc
                                  (and vdoc fdoc '(#\Newline #\Newline))
                                  vdoc))
                default))
          default))))


;;;; Package Commands

(defslimefun list-all-package-names (&optional nicknames)
  "Return a list of all package names.
Include the nicknames if NICKNAMES is true."
  (mapcar #'unparse-name
          (if nicknames
              (mapcan #'package-names (list-all-packages))
              (mapcar #'package-name  (list-all-packages)))))


;;;; Tracing

;; Use eval for the sake of portability... 
(defun tracedp (fspec)
  (member fspec (eval '(trace))))

(defslimefun swank-toggle-trace (spec-string)
  (let ((spec (from-string spec-string)))
    (cond ((consp spec) ; handle complicated cases in the backend
           (toggle-trace spec))
          ((tracedp spec)
	   (eval `(untrace ,spec))
	   (format nil "~S is now untraced." spec))
	  (t
           (eval `(trace ,spec))
	   (format nil "~S is now traced." spec)))))

(defslimefun untrace-all ()
  (untrace))

(defslimefun redirect-trace-output (target)
  (setf (connection.trace-output *emacs-connection*)
        (make-output-stream-for-target *emacs-connection* target))
  nil)


;;;; Undefing

(defslimefun undefine-function (fname-string)
  (let ((fname (from-string fname-string)))
    (format nil "~S" (fmakunbound fname))))


;;;; Profiling

(defun profiledp (fspec)
  (member fspec (profiled-functions)))

(defslimefun toggle-profile-fdefinition (fname-string)
  (let ((fname (from-string fname-string)))
    (cond ((profiledp fname)
	   (unprofile fname)
	   (format nil "~S is now unprofiled." fname))
	  (t
           (profile fname)
	   (format nil "~S is now profiled." fname)))))


;;;; Source Locations

(defslimefun find-definition-for-thing (thing)
  (find-source-location thing))

(defslimefun find-source-location-for-emacs (spec)
  (find-source-location (value-spec-ref spec)))

(defun value-spec-ref (spec)
  (destructure-case spec
    ((:string string package)
     (with-buffer-syntax (package)
       (eval (read-from-string string))))
    ((:inspector part) 
     (inspector-nth-part part))
    ((:sldb frame var)
     (frame-var-value frame var))))
  
(defslimefun find-definitions-for-emacs (name)
  "Return a list ((DSPEC LOCATION) ...) of definitions for NAME.
DSPEC is a string and LOCATION a source location. NAME is a string."
  (multiple-value-bind (sexp error) (ignore-errors (values (from-string name)))
    (unless error
      (mapcar #'xref>elisp (find-definitions sexp)))))

(defslimefun xref (type name)
  (let ((symbol (parse-symbol-or-lose name *buffer-package*)))
    (mapcar #'xref>elisp
            (ecase type
              (:calls (who-calls symbol))
              (:calls-who (calls-who symbol))
              (:references (who-references symbol))
              (:binds (who-binds symbol))
              (:sets (who-sets symbol))
              (:macroexpands (who-macroexpands symbol))
              (:specializes (who-specializes symbol))
              (:callers (list-callers symbol))
              (:callees (list-callees symbol))))))

(defun xref>elisp (xref)
  (destructuring-bind (name loc) xref
    (list (to-string name) loc)))


;;;; Inspecting

(defstruct (inspector-state (:conc-name istate.))
  object 
  (parts (make-array 10 :adjustable t :fill-pointer 0))
  (actions (make-array 10 :adjustable t :fill-pointer 0))
  content
  next previous)

(defvar *istate* nil)
(defvar *inspector-history*)

(defun reset-inspector ()
  (setq *istate* nil
        *inspector-history* (make-array 10 :adjustable t :fill-pointer 0)))
  
(defslimefun init-inspector (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLIME inspection request.")
      (reset-inspector)
      (inspect-object (eval (read-from-string string))))))

(defun inspect-object (o)
  (let ((previous *istate*)
        (content (emacs-inspect/printer-bindings o)))
    (unless (find o *inspector-history*)
      (vector-push-extend o *inspector-history*))
    (setq *istate* (make-inspector-state :object o :previous previous 
                                         :content content))
    (if previous (setf (istate.next previous) *istate*))
    (istate>elisp *istate*)))

(defun emacs-inspect/printer-bindings (object)
  (let ((*print-lines* 1) (*print-right-margin* 75)
        (*print-pretty* t) (*print-readably* nil))
    (emacs-inspect object)))

(defun istate>elisp (istate)
  (list :title (call/truncated-output-to-string
                200
                (lambda (s)
                  (print-unreadable-object ((istate.object istate)
                                           s :type t :identity t))))
        :id (assign-index (istate.object istate) (istate.parts istate))
        :content (prepare-range istate 0 500)))

(defun prepare-range (istate start end)
  (let* ((range (content-range (istate.content istate) start end))
         (ps (loop for part in range append (prepare-part part istate))))
    (list ps 
          (if (< (length ps) (- end start))
              (+ start (length ps))
              (+ end 1000))
          start end)))

(defun prepare-part (part istate)
  (let ((newline '#.(string #\newline)))
    (etypecase part
      (string (list part))
      (cons (destructure-case part
              ((:newline) (list newline))
              ((:value obj &optional str) 
               (list (value-part obj str (istate.parts istate))))
              ((:action label lambda &key (refreshp t)) 
               (list (action-part label lambda refreshp
                                  (istate.actions istate))))
              ((:line label value)
               (list (princ-to-string label) ": "
                     (value-part value nil (istate.parts istate))
                     newline)))))))

(defun value-part (object string parts)
  (list :value 
        (or string (print-part-to-string object))
        (assign-index object parts)))

(defun action-part (label lambda refreshp actions)
  (list :action label (assign-index (list lambda refreshp) actions)))

(defun assign-index (object vector)
  (let ((index (fill-pointer vector)))
    (vector-push-extend object vector)
    index))

(defun print-part-to-string (value)
  (let* ((string (to-line value))
         (pos (position value *inspector-history*)))
    (if pos
        (format nil "#~D=~A" pos string)
        string)))

;; Print OBJECT to a single line. Return the string.
(defun to-line  (object &optional (width 75))
  (call/truncated-output-to-string
   width
   (lambda (*standard-output*)
     (write object :right-margin width :lines 1))
   ".."))

(defun content-range (list start end)
  (typecase list
    (list (let ((len (length list)))
            (subseq list start (min len end))))
    (lcons (llist-range list start end))))

(defslimefun inspector-nth-part (index)
  (aref (istate.parts *istate*) index))

(defslimefun inspect-nth-part (index)
  (with-buffer-syntax ()
    (inspect-object (inspector-nth-part index))))

(defslimefun inspector-range (from to)
  (prepare-range *istate* from to))

(defslimefun inspector-call-nth-action (index &rest args)
  (destructuring-bind (fun refreshp) (aref (istate.actions *istate*) index)
    (apply fun args)
    (if refreshp
        (inspector-reinspect)
        ;; tell emacs that we don't want to refresh the inspector buffer
        nil)))

(defslimefun inspector-pop ()
  "Inspect the previous object.
Return nil if there's no previous object."
  (with-buffer-syntax ()
    (cond ((istate.previous *istate*)
           (setq *istate* (istate.previous *istate*))
           (istate>elisp *istate*))
          (t nil))))

(defslimefun inspector-next ()
  "Inspect the next element in the history of inspected objects.."
  (with-buffer-syntax ()
    (cond ((istate.next *istate*)
           (setq *istate* (istate.next *istate*))
           (istate>elisp *istate*))
          (t nil))))

(defslimefun inspector-reinspect ()
  (setf (istate.content *istate*)
        (emacs-inspect/printer-bindings (istate.object *istate*)))
  (istate>elisp *istate*))

(defslimefun quit-inspector ()
  (reset-inspector)
  nil)

(defslimefun describe-inspectee ()
  "Describe the currently inspected object."
  (with-buffer-syntax ()
    (describe-to-string (istate.object *istate*))))

(defslimefun pprint-inspector-part (index)
  "Pretty-print the currently inspected object."
  (with-buffer-syntax ()
    (swank-pprint (list (inspector-nth-part index)))))

(defslimefun inspect-in-frame (string index)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLIME inspection request.")
      (reset-inspector)
      (inspect-object (eval-in-frame (from-string string) index)))))

(defslimefun inspect-current-condition ()
  (with-buffer-syntax ()
    (reset-inspector)
    (inspect-object *swank-debugger-condition*)))

(defslimefun inspect-frame-var (frame var)
  (with-buffer-syntax ()
    (reset-inspector)
    (inspect-object (frame-var-value frame var))))

;;;;; Lazy lists 

(defstruct (lcons (:constructor %lcons (car %cdr))
                  (:predicate lcons?))
  car 
  (%cdr nil :type (or null lcons function))
  (forced? nil))

(defmacro lcons (car cdr)
  `(%lcons ,car (lambda () ,cdr)))

(defmacro lcons* (car cdr &rest more)
  (cond ((null more) `(lcons ,car ,cdr))
        (t `(lcons ,car (lcons* ,cdr ,@more)))))

(defun lcons-cdr (lcons)
  (with-struct* (lcons- @ lcons) 
    (cond ((@ forced?)
           (@ %cdr))
          (t
           (let ((value (funcall (@ %cdr))))
             (setf (@ forced?) t
                   (@ %cdr) value))))))

(defun llist-range (llist start end)
  (llist-take (llist-skip llist start) (- end start)))

(defun llist-skip (lcons index)
  (do ((i 0 (1+ i))
       (l lcons (lcons-cdr l)))
      ((or (= i index) (null l))
       l)))

(defun llist-take (lcons count)
  (let ((result '()))
    (do ((i 0 (1+ i))
         (l lcons (lcons-cdr l)))
        ((or (= i count)
             (null l)))
      (push (lcons-car l) result))
    (nreverse result)))

(defun iline (label value)
  `(:line ,label ,value))

;;;;; Lists

(defmethod emacs-inspect ((o cons))
  (if (listp (cdr o))
      (inspect-list o)
      (inspect-cons o)))

(defun inspect-cons (cons)
  (label-value-line* 
   ('car (car cons))
   ('cdr (cdr cons))))

(defun inspect-list (list)
  (multiple-value-bind (length tail) (safe-length list)
    (flet ((frob (title list)
             (list* title '(:newline) (inspect-list-aux list))))
      (cond ((not length)
             (frob "A circular list:"
                   (cons (car list)
                         (ldiff (cdr list) list))))
            ((not tail)
             (frob "A proper list:" list))
            (t
             (frob "An improper list:" list))))))

(defun inspect-list-aux (list)
  (loop for i from 0  for rest on list  while (consp rest)  append 
        (if (listp (cdr rest))
            (label-value-line i (car rest))
            (label-value-line* (i (car rest)) (:tail (cdr rest))))))

(defun safe-length (list)
  "Similar to `list-length', but avoid errors on improper lists.
Return two values: the length of the list and the last cdr.
Return NIL if LIST is circular."
  (do ((n 0 (+ n 2))                    ;Counter.
       (fast list (cddr fast))          ;Fast pointer: leaps by 2.
       (slow list (cdr slow)))          ;Slow pointer: leaps by 1.
      (nil)
    (cond ((null fast) (return (values n nil)))
          ((not (consp fast)) (return (values n fast)))
          ((null (cdr fast)) (return (values (1+ n) (cdr fast))))
          ((and (eq fast slow) (> n 0)) (return nil))
          ((not (consp (cdr fast))) (return (values (1+ n) (cdr fast)))))))

;;;;; Hashtables



(defmethod emacs-inspect ((ht hash-table))
  (append
   (label-value-line*
    ("Count" (hash-table-count ht))
    ("Size" (hash-table-size ht))
    ("Test" (hash-table-test ht))
    ("Rehash size" (hash-table-rehash-size ht))
    ("Rehash threshold" (hash-table-rehash-threshold ht)))
   (let ((weakness (hash-table-weakness ht)))
     (when weakness
       (label-value-line "Weakness:" weakness)))
   (unless (zerop (hash-table-count ht))
     `((:action "[clear hashtable]" 
                ,(lambda () (clrhash ht))) (:newline)
       "Contents: " (:newline)))
   (loop for key being the hash-keys of ht
         for value being the hash-values of ht
         append `((:value ,key) " = " (:value ,value)
                  " " (:action "[remove entry]"
                               ,(let ((key key))
                                     (lambda () (remhash key ht))))
                  (:newline)))))

;;;;; Arrays

(defmethod emacs-inspect ((array array))
  (lcons*
   (iline "Dimensions" (array-dimensions array))
   (iline "Element type" (array-element-type array))
   (iline "Total size" (array-total-size array))
   (iline "Adjustable" (adjustable-array-p array))
   (iline "Fill pointer" (if (array-has-fill-pointer-p array)
                             (fill-pointer array)))
   "Contents:" '(:newline)
   (labels ((k (i max)
              (cond ((= i max) '())
                    (t (lcons (iline i (row-major-aref array i))
                              (k (1+ i) max))))))
     (k 0 (array-total-size array)))))

;;;;; Chars

(defmethod emacs-inspect ((char character))
  (append 
   (label-value-line*
    ("Char code" (char-code char))
    ("Lower cased" (char-downcase char))
    ("Upper cased" (char-upcase char)))
   (if (get-macro-character char)
       `("In the current readtable (" 
         (:value ,*readtable*) ") it is a macro character: "
         (:value ,(get-macro-character char))))))

;;;; Thread listing

(defvar *thread-list* ()
  "List of threads displayed in Emacs.  We don't care a about
synchronization issues (yet).  There can only be one thread listing at
a time.")

(defslimefun list-threads ()
  "Return a list ((ID NAME STATUS DESCRIPTION) ...) of all threads."
  (setq *thread-list* (all-threads))
  (loop for thread in  *thread-list* 
       for name = (thread-name thread)
        collect (list (thread-id thread)
                      (if (symbolp name) (symbol-name name) name)
                      (thread-status thread)
                      (thread-description thread)
                      )))

(defslimefun quit-thread-browser ()
  (setq *thread-list* nil))

(defun nth-thread (index)
  (nth index *thread-list*))

(defslimefun debug-nth-thread (index)
  (let ((connection *emacs-connection*))
    (interrupt-thread (nth-thread index)
                      (lambda ()
                        (invoke-or-queue-interrupt
                         (lambda ()
                           (with-connection (connection)
                             (simple-break))))))))

(defslimefun kill-nth-thread (index)
  (kill-thread (nth-thread index)))

(defslimefun start-swank-server-in-thread (index port-file-name)
  "Interrupt the INDEXth thread and make it start a swank server.
The server port is written to PORT-FILE-NAME."
  (interrupt-thread (nth-thread index)
                    (lambda ()
                      (start-server port-file-name :style nil))))

;;;; Class browser

(defun mop-helper (class-name fn)
  (let ((class (find-class class-name nil)))
    (if class
        (mapcar (lambda (x) (to-string (class-name x)))
                (funcall fn class)))))

(defslimefun mop (type symbol-name)
  "Return info about classes using mop.

    When type is:
     :subclasses - return the list of subclasses of class.
     :superclasses - return the list of superclasses of class."
  (let ((symbol (parse-symbol symbol-name *buffer-package*)))
    (ecase type
      (:subclasses
       (mop-helper symbol #'swank-mop:class-direct-subclasses))
      (:superclasses 
       (mop-helper symbol #'swank-mop:class-direct-superclasses)))))


;;;; Automatically synchronized state
;;;
;;; Here we add hooks to push updates of relevant information to
;;; Emacs.

;;;;; *FEATURES*

(defun sync-features-to-emacs ()
  "Update Emacs if any relevant Lisp state has changed."
  ;; FIXME: *slime-features* should be connection-local
  (unless (eq *slime-features* *features*)
    (setq *slime-features* *features*)
    (send-to-emacs (list :new-features (features-for-emacs)))))

(defun features-for-emacs ()
  "Return `*slime-features*' in a format suitable to send it to Emacs."
  *slime-features*)

(add-hook *pre-reply-hook* 'sync-features-to-emacs)


;;;;; Indentation of macros
;;;
;;; This code decides how macros should be indented (based on their
;;; arglists) and tells Emacs. A per-connection cache is used to avoid
;;; sending redundant information to Emacs -- we just say what's
;;; changed since last time.
;;;
;;; The strategy is to scan all symbols, pick out the macros, and look
;;; for &body-arguments.

(defvar *configure-emacs-indentation* t
  "When true, automatically send indentation information to Emacs
after each command.")

(defslimefun update-indentation-information ()
  (perform-indentation-update *emacs-connection* t)
  nil)

;; This function is for *PRE-REPLY-HOOK*.
(defun sync-indentation-to-emacs ()
  "Send any indentation updates to Emacs via CONNECTION."
  (when *configure-emacs-indentation*
    (let ((fullp (need-full-indentation-update-p *emacs-connection*)))
      (perform-indentation-update *emacs-connection* fullp))))

(defun need-full-indentation-update-p (connection)
  "Return true if the whole indentation cache should be updated.
This is a heuristic to avoid scanning all symbols all the time:
instead, we only do a full scan if the set of packages has changed."
  (set-difference (list-all-packages)
                  (connection.indentation-cache-packages connection)))

(defun perform-indentation-update (connection force)
  "Update the indentation cache in CONNECTION and update Emacs.
If FORCE is true then start again without considering the old cache."
  (let ((cache (connection.indentation-cache connection)))
    (when force (clrhash cache))
    (let ((delta (update-indentation/delta-for-emacs cache force)))
      (setf (connection.indentation-cache-packages connection)
            (list-all-packages))
      (unless (null delta)
        (send-to-emacs (list :indentation-update delta))))))

(defun update-indentation/delta-for-emacs (cache &optional force)
  "Update the cache and return the changes in a (SYMBOL . INDENT) list.
If FORCE is true then check all symbols, otherwise only check symbols
belonging to the buffer package."
  (let ((alist '()))
      (flet ((consider (symbol)
             (let ((indent (symbol-indentation symbol)))
               (when indent
                 (unless (equal (gethash symbol cache) indent)
                   (setf (gethash symbol cache) indent)
                   (push (cons (string-downcase symbol) indent) alist))))))
      (if force
          (do-all-symbols (symbol)
            (consider symbol))
          (do-symbols (symbol *buffer-package*)
            ;; We're really just interested in the symbols of *BUFFER-PACKAGE*,
            ;; and *not* all symbols that are _present_ (cf. SYMBOL-STATUS.)
            (when (eq (symbol-package symbol) *buffer-package*)
              (consider symbol)))))
    alist))

(defun package-names (package)
  "Return the name and all nicknames of PACKAGE in a fresh list."
  (cons (package-name package) (copy-list (package-nicknames package))))

(defun cl-symbol-p (symbol)
  "Is SYMBOL a symbol in the COMMON-LISP package?"
  (eq (symbol-package symbol) cl-package))

(defun known-to-emacs-p (symbol)
  "Return true if Emacs has special rules for indenting SYMBOL."
  (cl-symbol-p symbol))

(defun symbol-indentation (symbol)
  "Return a form describing the indentation of SYMBOL.
The form is to be used as the `common-lisp-indent-function' property
in Emacs."
  (if (and (macro-function symbol)
           (not (known-to-emacs-p symbol)))
      (let ((arglist (arglist symbol)))
        (etypecase arglist
          ((member :not-available)
           nil)
          (list
           (macro-indentation arglist))))
      nil))

(defun macro-indentation (arglist)
  (if (well-formed-list-p arglist)
      (position '&body (remove '&optional (clean-arglist arglist)))
      nil))

(defun clean-arglist (arglist)
  "Remove &whole, &enviroment, and &aux elements from ARGLIST."
  (cond ((null arglist) '())
        ((member (car arglist) '(&whole &environment))
         (clean-arglist (cddr arglist)))
        ((eq (car arglist) '&aux)
         '())
        (t (cons (car arglist) (clean-arglist (cdr arglist))))))

(defun well-formed-list-p (list)
  "Is LIST a proper list terminated by NIL?"
  (typecase list
    (null t)
    (cons (well-formed-list-p (cdr list)))
    (t    nil)))

(defun print-indentation-lossage (&optional (stream *standard-output*))
  "Return the list of symbols whose indentation styles collide incompatibly.
Collisions are caused because package information is ignored."
  (let ((table (make-hash-table :test 'equal)))
    (flet ((name (s) (string-downcase (symbol-name s))))
      (do-all-symbols (s)
        (setf (gethash (name s) table)
              (cons s (symbol-indentation s))))
      (let ((collisions '()))
        (do-all-symbols (s)
          (let* ((entry (gethash (name s) table))
                 (owner (car entry))
                 (indent (cdr entry)))
            (unless (or (eq s owner)
                        (equal (symbol-indentation s) indent)
                        (and (not (fboundp s))
                             (null (macro-function s))))
              (pushnew owner collisions)
              (pushnew s collisions))))
        (if (null collisions)
            (format stream "~&No worries!~%")
            (format stream "~&Symbols with collisions:~%~{  ~S~%~}"
                    collisions))))))

(add-hook *pre-reply-hook* 'sync-indentation-to-emacs)

(defun before-init (version load-path)
  (setq *swank-wire-protocol-version* version)
  (setq *load-path* load-path)
  (swank-backend::warn-unimplemented-interfaces))

(defun init ()
  (run-hook *after-init-hook*))

;;; swank.lisp ends here