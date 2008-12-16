;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; swank-sbcl.lisp --- SLIME backend for SBCL.
;;;
;;; Created 2003, Daniel Barlow <dan@metacircles.com>
;;;
;;; This code has been placed in the Public Domain.  All warranties are
;;; disclaimed.

;;; Requires the SB-INTROSPECT contrib.

;;; Administrivia

(in-package :swank-backend)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-bsd-sockets)
  (require 'sb-introspect)
  (require 'sb-posix)
  (require 'sb-cltl2))

(declaim (optimize (debug 2) 
                   (sb-c::insert-step-conditions 0)
                   (sb-c::insert-debug-catch 0)
                   (sb-c::merge-tail-calls 2)))

(import-from :sb-gray *gray-stream-symbols* :swank-backend)

;;; backwards compability tests

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Generate a form suitable for testing for stepper support (0.9.17)
  ;; with #+.
  (defun sbcl-with-new-stepper-p ()
    (if (find-symbol "ENABLE-STEPPING" "SB-IMPL")
        '(:and)
        '(:or)))
  ;; Ditto for weak hash-tables
  (defun sbcl-with-weak-hash-tables ()
    (if (find-symbol "HASH-TABLE-WEAKNESS" "SB-EXT")
        '(:and)
        '(:or)))
  ;; And for xref support (1.0.1)
  (defun sbcl-with-xref-p ()
    (if (find-symbol "WHO-CALLS" "SB-INTROSPECT")
        '(:and)
        '(:or)))
  ;; ... for restart-frame support (1.0.2)
  (defun sbcl-with-restart-frame ()
    (if (find-symbol "FRAME-HAS-DEBUG-TAG-P" "SB-DEBUG")
        '(:and)
        '(:or)))
  (defun sbcl-with-symbol (name package)
    (if (find-symbol (string name) (string package))
        '(:and)
        '(:or)))
  )

;;; swank-mop

(import-swank-mop-symbols :sb-mop '(:slot-definition-documentation))

(defun swank-mop:slot-definition-documentation (slot)
  (sb-pcl::documentation slot t))

;;; Connection info

(defimplementation lisp-implementation-type-name ()
  "sbcl")

;; Declare return type explicitly to shut up STYLE-WARNINGS about
;; %SAP-ALIEN in ENABLE-SIGIO-ON-FD below.
(declaim (ftype (function () (values (signed-byte 32) &optional)) getpid))
(defimplementation getpid ()
  (sb-posix:getpid))

;;; TCP Server

(defimplementation preferred-communication-style ()
  (cond
    ;; fixme: when SBCL/win32 gains better select() support, remove
    ;; this.
    ((member :win32 *features*) nil)
    ((member :sb-thread *features*) :spawn)
    (t :fd-handler)))

(defun resolve-hostname (name)
  (car (sb-bsd-sockets:host-ent-addresses
        (sb-bsd-sockets:get-host-by-name name))))

(defimplementation create-socket (host port)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket (resolve-hostname host) port)
    (sb-bsd-sockets:socket-listen socket 5)
    socket))

(defimplementation local-port (socket)
  (nth-value 1 (sb-bsd-sockets:socket-name socket)))

(defimplementation close-socket (socket)
  (sb-sys:invalidate-descriptor (socket-fd socket))
  (sb-bsd-sockets:socket-close socket))

(defimplementation accept-connection (socket &key
                                      external-format
                                      buffering timeout)
  (declare (ignore timeout))
  (make-socket-io-stream (accept socket)
                         (or external-format :iso-latin-1-unix)
                         (or buffering :full)))

#-win32
(defimplementation install-sigint-handler (function)
  (sb-sys:enable-interrupt sb-unix:sigint 
                           (lambda (&rest args)
                             (declare (ignore args))
                             (sb-sys:invoke-interruption 
                              (lambda ()
                                (sb-sys:with-interrupts 
                                  (funcall function)))))))

(defvar *sigio-handlers* '()
  "List of (key . fn) pairs to be called on SIGIO.")

(defun sigio-handler (signal code scp)
  (declare (ignore signal code scp))
  (mapc (lambda (handler)
          (funcall (the function (cdr handler))))
        *sigio-handlers*))

(defun set-sigio-handler ()
  (sb-sys:enable-interrupt sb-unix:sigio (lambda (signal code scp)
                                           (sigio-handler signal code scp))))

(defun enable-sigio-on-fd (fd)
  (sb-posix::fcntl fd sb-posix::f-setfl sb-posix::o-async)
  (sb-posix::fcntl fd sb-posix::f-setown (getpid))
  (values))

(defimplementation add-sigio-handler (socket fn)
  (set-sigio-handler)
  (let ((fd (socket-fd socket)))
    (enable-sigio-on-fd fd)
    (push (cons fd fn) *sigio-handlers*)))

(defimplementation remove-sigio-handlers (socket)
  (let ((fd (socket-fd socket)))
    (setf *sigio-handlers* (delete fd *sigio-handlers* :key #'car))
    (sb-sys:invalidate-descriptor fd))
  (close socket))

(defimplementation add-fd-handler (socket fn)
  (declare (type function fn))
  (let ((fd (socket-fd socket)))
    (sb-sys:add-fd-handler fd :input (lambda (_)
                                       _
                                       (funcall fn)))))

(defimplementation remove-fd-handlers (socket)
  (sb-sys:invalidate-descriptor (socket-fd socket)))

(defun socket-fd (socket)
  (etypecase socket
    (fixnum socket)
    (sb-bsd-sockets:socket (sb-bsd-sockets:socket-file-descriptor socket))
    (file-stream (sb-sys:fd-stream-fd socket))))

(defvar *wait-for-input-called*)

(defimplementation wait-for-input (streams &optional timeout)
  (assert (member timeout '(nil t)))
  (when (boundp '*wait-for-input-called*)
    (setq *wait-for-input-called* t))
  (let ((*wait-for-input-called* nil))
    (loop
     (let ((ready (remove-if-not #'listen streams)))
       (when ready (return ready)))
     (when timeout (return nil))
     (when (check-slime-interrupts) (return :interrupt))
     (when *wait-for-input-called* (return :interrupt))
     (let* ((f (constantly t))
            (handlers (loop for s in streams
                            collect (add-one-shot-handler s f))))
       (unwind-protect
            (sb-sys:serve-event 0.2)
         (mapc #'sb-sys:remove-fd-handler handlers))))))

(defun add-one-shot-handler (stream function)
  (let (handler)
    (setq handler 
          (sb-sys:add-fd-handler (sb-sys:fd-stream-fd stream) :input
                                 (lambda (fd)
                                   (declare (ignore fd))
                                   (sb-sys:remove-fd-handler handler)
                                   (funcall function stream))))))

(defvar *external-format-to-coding-system*
  '((:iso-8859-1 
     "latin-1" "latin-1-unix" "iso-latin-1-unix" 
     "iso-8859-1" "iso-8859-1-unix")
    (:utf-8 "utf-8" "utf-8-unix")
    (:euc-jp "euc-jp" "euc-jp-unix")
    (:us-ascii "us-ascii" "us-ascii-unix")))

;; C.f. R.M.Kreuter in <20536.1219412774@progn.net> on sbcl-general, 2008-08-22.
(defvar *physical-pathname-host* (pathname-host (user-homedir-pathname)))

(defimplementation parse-emacs-filename (filename)
  (sb-ext:parse-native-namestring filename *physical-pathname-host*))

(defimplementation find-external-format (coding-system)
  (car (rassoc-if (lambda (x) (member coding-system x :test #'equal))
                  *external-format-to-coding-system*)))

(defun make-socket-io-stream (socket external-format buffering)
  (sb-bsd-sockets:socket-make-stream socket
                                     :output t
                                     :input t
                                     :element-type 'character
                                     :buffering buffering
                                     #+sb-unicode :external-format
                                     #+sb-unicode external-format
                                     ))

(defun accept (socket)
  "Like socket-accept, but retry on EAGAIN."
  (loop (handler-case
            (return (sb-bsd-sockets:socket-accept socket))
          (sb-bsd-sockets:interrupted-error ()))))

(defimplementation call-without-interrupts (fn)
  (declare (type function fn))
  (sb-sys:without-interrupts (funcall fn)))



;;;; Support for SBCL syntax

;;; SBCL's source code is riddled with #! reader macros.  Also symbols
;;; containing `!' have special meaning.  We have to work long and
;;; hard to be able to read the source.  To deal with #! reader
;;; macros, we use a special readtable.  The special symbols are
;;; converted by a condition handler.

(defun feature-in-list-p (feature list)
  (etypecase feature
    (symbol (member feature list :test #'eq))
    (cons (flet ((subfeature-in-list-p (subfeature)
		   (feature-in-list-p subfeature list)))
	    (ecase (first feature)
	      (:or  (some  #'subfeature-in-list-p (rest feature)))
	      (:and (every #'subfeature-in-list-p (rest feature)))
	      (:not (destructuring-bind (e) (cdr feature)
                      (not (subfeature-in-list-p e)))))))))

(defun shebang-reader (stream sub-character infix-parameter)
  (declare (ignore sub-character))
  (when infix-parameter
    (error "illegal read syntax: #~D!" infix-parameter))
  (let ((next-char (read-char stream)))
    (unless (find next-char "+-")
      (error "illegal read syntax: #!~C" next-char))
    ;; When test is not satisfied
    ;; FIXME: clearer if order of NOT-P and (NOT NOT-P) were reversed? then
    ;; would become "unless test is satisfied"..
    (when (let* ((*package* (find-package "KEYWORD"))
		 (*read-suppress* nil)
		 (not-p (char= next-char #\-))
		 (feature (read stream)))
	    (if (feature-in-list-p feature *features*)
		not-p
		(not not-p)))
      ;; Read (and discard) a form from input.
      (let ((*read-suppress* t))
	(read stream t nil t))))
 (values))

(defvar *shebang-readtable*
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\!
                                  (lambda (s c n) (shebang-reader s c n))
                                  *readtable*)
    *readtable*))

(defun shebang-readtable ()
  *shebang-readtable*)

(defun sbcl-package-p (package)
  (let ((name (package-name package)))
    (eql (mismatch "SB-" name) 3)))

(defun sbcl-source-file-p (filename)
  (when filename
    (loop for (nil pattern) in (logical-pathname-translations "SYS")
          thereis (pathname-match-p filename pattern))))

(defun guess-readtable-for-filename (filename)
  (if (sbcl-source-file-p filename)
      (shebang-readtable)
      *readtable*))

(defvar *debootstrap-packages* t)

(defun call-with-debootstrapping (fun)
  (handler-bind ((sb-int:bootstrap-package-not-found
                  #'sb-int:debootstrap-package))
    (funcall fun)))

(defmacro with-debootstrapping (&body body)
  `(call-with-debootstrapping (lambda () ,@body)))

(defimplementation call-with-syntax-hooks (fn)
  (cond ((and *debootstrap-packages*
              (sbcl-package-p *package*))
         (with-debootstrapping (funcall fn)))
        (t
         (funcall fn))))

(defimplementation default-readtable-alist ()
  (let ((readtable (shebang-readtable)))
    (loop for p in (remove-if-not #'sbcl-package-p (list-all-packages))
          collect (cons (package-name p) readtable))))

;;; Utilities

(defimplementation arglist (fname)
  (sb-introspect:function-arglist fname))

(defimplementation function-name (f)
  (check-type f function)
  (sb-impl::%fun-name f))

(defmethod declaration-arglist ((decl-identifier (eql 'optimize)))
  (flet ((ensure-list (thing) (if (listp thing) thing (list thing))))
    (let* ((flags (sb-cltl2:declaration-information decl-identifier)))
      (if flags
          ;; Symbols aren't printed with package qualifiers, but the FLAGS would
          ;; have to be fully qualified when used inside a declaration. So we
          ;; strip those as long as there's no better way. (FIXME)
          `(&any ,@(remove-if-not #'(lambda (qualifier)
                                      (find-symbol (symbol-name (first qualifier)) :cl))
                                  flags :key #'ensure-list))
          (call-next-method)))))

(defvar *buffer-name* nil)
(defvar *buffer-offset*)
(defvar *buffer-substring* nil)

(defvar *previous-compiler-condition* nil
  "Used to detect duplicates.")

(defun handle-notification-condition (condition)
  "Handle a condition caused by a compiler warning.
This traps all compiler conditions at a lower-level than using
C:*COMPILER-NOTIFICATION-FUNCTION*. The advantage is that we get to
craft our own error messages, which can omit a lot of redundant
information."
  (unless (or (eq condition *previous-compiler-condition*))
    ;; First resignal warnings, so that outer handlers -- which may choose to
    ;; muffle this -- get a chance to run.
    (when (typep condition 'warning)
      (signal condition))
    (setq *previous-compiler-condition* condition)
    (signal-compiler-condition condition (sb-c::find-error-context nil))))

(defun signal-compiler-condition (condition context)
  (signal (make-condition
           'compiler-condition
           :original-condition condition
           :severity (etypecase condition
                       (sb-c:compiler-error  :error)
                       (sb-ext:compiler-note :note)
                       (style-warning        :style-warning)
                       (warning              :warning)
                       (error                :error))
           :short-message (brief-compiler-message-for-emacs condition)
           :references (condition-references (real-condition condition))
           :message (long-compiler-message-for-emacs condition context)
           :location (compiler-note-location context))))

(defun real-condition (condition)
  "Return the encapsulated condition or CONDITION itself."
  (typecase condition
    (sb-int:encapsulated-condition (sb-int:encapsulated-condition condition))
    (t condition)))

(defun condition-references (condition)
  (if (typep condition 'sb-int:reference-condition)
      (externalize-reference
       (sb-int:reference-condition-references condition))))

(defun compiler-note-location (context)
  (if context
      (locate-compiler-note
       (sb-c::compiler-error-context-file-name context)
       (compiler-source-path context)
       (sb-c::compiler-error-context-original-source context))
      (list :error "No error location available")))

(defun locate-compiler-note (file source-path source)
  (cond ((and (not (eq file :lisp)) *buffer-name*)
         ;; Compiling from a buffer
         (make-location (list :buffer *buffer-name*)
                        (list :offset  *buffer-offset* 
                              (source-path-string-position
                               source-path *buffer-substring*))))
        ((and (pathnamep file) (null *buffer-name*))
         ;; Compiling from a file
         (make-location (list :file (namestring file))
                        (list :position (1+ (source-path-file-position
                                             source-path file)))))
        ((and (eq file :lisp) (stringp source))
         ;; Compiling macro generated code
         (make-location (list :source-form source)
                        (list :position 1)))
        (t
         (error "unhandled case in compiler note ~S ~S ~S" file source-path source))))

(defun brief-compiler-message-for-emacs (condition)
  "Briefly describe a compiler error for Emacs.
When Emacs presents the message it already has the source popped up
and the source form highlighted. This makes much of the information in
the error-context redundant."
  (let ((sb-int:*print-condition-references* nil))
    (princ-to-string condition)))

(defun long-compiler-message-for-emacs (condition error-context)
  "Describe a compiler error for Emacs including context information."
  (declare (type (or sb-c::compiler-error-context null) error-context))
  (multiple-value-bind (enclosing source)
      (if error-context
          (values (sb-c::compiler-error-context-enclosing-source error-context)
                  (sb-c::compiler-error-context-source error-context)))
    (let ((sb-int:*print-condition-references* nil))
      (format nil "~@[--> ~{~<~%--> ~1:;~A~> ~}~%~]~@[~{==>~%~A~%~}~]~A"
              enclosing source condition))))

(defun compiler-source-path (context)
  "Return the source-path for the current compiler error.
Returns NIL if this cannot be determined by examining internal
compiler state."
  (cond ((sb-c::node-p context)
         (reverse
          (sb-c::source-path-original-source
           (sb-c::node-source-path context))))
        ((sb-c::compiler-error-context-p context)
         (reverse
          (sb-c::compiler-error-context-original-source-path context)))))

(defimplementation call-with-compilation-hooks (function)
  (declare (type function function))
  (handler-bind ((sb-c:fatal-compiler-error #'handle-file-compiler-termination)
                 (sb-c:compiler-error  #'handle-notification-condition)
                 (sb-ext:compiler-note #'handle-notification-condition)
                 (warning              #'handle-notification-condition))
    (funcall function)))

(defun handle-file-compiler-termination (condition)
  "Handle a condition that caused the file compiler to terminate."
  (handle-notification-condition
   (sb-int:encapsulated-condition condition)))

(defvar *trap-load-time-warnings* nil)

(defimplementation swank-compile-file (pathname load-p external-format)
  (handler-case
      (let ((output-file (with-compilation-hooks ()
                           (compile-file pathname 
                                         :external-format external-format))))
        (when output-file
          ;; Cache the latest source file for definition-finding.
          (source-cache-get pathname (file-write-date pathname))
          (when load-p
            (load output-file))))
    (sb-c:fatal-compiler-error () nil)))

;;;; compile-string

;;; We copy the string to a temporary file in order to get adequate
;;; semantics for :COMPILE-TOPLEVEL and :LOAD-TOPLEVEL EVAL-WHEN forms
;;; which the previous approach using
;;;     (compile nil `(lambda () ,(read-from-string string)))
;;; did not provide.

(sb-alien:define-alien-routine "tmpnam" sb-alien:c-string
  (dest (* sb-alien:c-string)))

(defun temp-file-name ()
  "Return a temporary file name to compile strings into."
  (concatenate 'string (tmpnam nil) ".lisp"))

(defimplementation swank-compile-string (string &key buffer position directory
                                                debug)
  (declare (ignorable debug))
  (let ((*buffer-name* buffer)
        (*buffer-offset* position)
        (*buffer-substring* string)
        (filename (temp-file-name))
        #+#.(swank-backend::sbcl-with-symbol 'restrict-compiler-policy 'sb-ext)
        (old-min-debug (assoc 'debug (sb-ext:restrict-compiler-policy)))
        )
    #+#.(swank-backend::sbcl-with-symbol 'restrict-compiler-policy 'sb-ext)
    (when debug
      (sb-ext:restrict-compiler-policy 'debug debug))
    (flet ((load-it (filename)
             (when filename (load filename)))
           (compile-it (cont)
             (with-compilation-hooks ()
               (with-compilation-unit
                   (:source-plist (list :emacs-buffer buffer
                                        :emacs-directory directory
                                        :emacs-string string
                                        :emacs-position position))
                 (funcall cont (compile-file filename))))))
      (with-open-file (s filename :direction :output :if-exists :error)
        (write-string string s))
      (unwind-protect
           (if *trap-load-time-warnings*
               (compile-it #'load-it)
               (load-it (compile-it #'identity)))
        (ignore-errors
          #+#.(swank-backend::sbcl-with-symbol
               'restrict-compiler-policy 'sb-ext)
          (sb-ext:restrict-compiler-policy 'debug (or old-min-debug 0))
          (delete-file filename)
          (delete-file (compile-file-pathname filename)))))))

;;;; Definitions

(defvar *debug-definition-finding* nil
  "When true don't handle errors while looking for definitions.
This is useful when debugging the definition-finding code.")

(defparameter *definition-types*
  '(:variable defvar
    :constant defconstant
    :type deftype
    :symbol-macro define-symbol-macro
    :macro defmacro
    :compiler-macro define-compiler-macro
    :function defun
    :generic-function defgeneric
    :method defmethod
    :setf-expander define-setf-expander
    :structure defstruct
    :condition define-condition
    :class defclass
    :method-combination define-method-combination
    :package defpackage
    :transform :deftransform
    :optimizer :defoptimizer
    :vop :define-vop
    :source-transform :define-source-transform)
  "Map SB-INTROSPECT definition type names to Slime-friendly forms")

(defimplementation find-definitions (name)
  (loop for type in *definition-types* by #'cddr
        for locations = (sb-introspect:find-definition-sources-by-name
                         name type)
        append (loop for source-location in locations collect
                     (make-source-location-specification type name
                                                         source-location))))

(defimplementation find-source-location (obj)
  (flet ((general-type-of (obj)
           (typecase obj
             (method             :method)
             (generic-function   :generic-function)
             (function           :function)
             (structure-class    :structure-class)
             (class              :class)
             (method-combination :method-combination)
             (package            :package)
             (condition          :condition)             
             (structure-object   :structure-object)
             (standard-object    :standard-object)
             (t                  :thing)))
         (to-string (obj)
           (typecase obj
             (package (princ-to-string obj)) ; Packages are possibly named entities.
             ((or structure-object standard-object condition)
              (with-output-to-string (s)
                (print-unreadable-object (obj s :type t :identity t))))
             (t (princ-to-string obj)))))
    (handler-case
        (make-definition-source-location
         (sb-introspect:find-definition-source obj) (general-type-of obj) (to-string obj))
      (error (e)
        (list :error (format nil "Error: ~A" e))))))


(defun make-source-location-specification (type name source-location)
  (list (list* (getf *definition-types* type)
               name
               (sb-introspect::definition-source-description source-location))
        (if *debug-definition-finding*
            (make-definition-source-location source-location type name)
            (handler-case
                (make-definition-source-location source-location type name)
              (error (e)
                (list :error (format nil "Error: ~A" e)))))))

(defun make-definition-source-location (definition-source type name)
  (with-struct (sb-introspect::definition-source-
                   pathname form-path character-offset plist
                   file-write-date)
      definition-source
    (destructuring-bind (&key emacs-buffer emacs-position emacs-directory
                              emacs-string &allow-other-keys)
        plist
      (cond
        (emacs-buffer
         (let* ((*readtable* (guess-readtable-for-filename emacs-directory))
                (pos (if form-path
                         (with-debootstrapping
                           (source-path-string-position form-path emacs-string))
                         character-offset))
                (snippet (string-path-snippet emacs-string form-path pos)))
           (make-location `(:buffer ,emacs-buffer)
                          `(:offset ,emacs-position ,pos)
                          `(:snippet ,snippet))))
        ((not pathname)
         `(:error ,(format nil "Source definition of ~A ~A not found"
                           (string-downcase type) name)))
        (t
         (let* ((namestring (namestring (translate-logical-pathname pathname)))
                (pos (source-file-position namestring file-write-date form-path
                                           character-offset))
                (snippet (source-hint-snippet namestring file-write-date pos)))
           (make-location `(:file ,namestring)
                          ;; /file positions/ in Common Lisp start
                          ;; from 0, in Emacs they start from 1.
                          `(:position ,(1+ pos))
                          `(:snippet ,snippet))))))))

(defun string-path-snippet (string form-path position)
  (if form-path
      ;; If we have a form-path, use it to derive a more accurate
      ;; snippet, so that we can point to the individual form rather
      ;; than just the toplevel form.
      (multiple-value-bind (data end)
          (let ((*read-suppress* t))
            (read-from-string string nil nil :start position))
        (declare (ignore data))
        (subseq string position end))
      string))    
    
(defun source-file-position (filename write-date form-path character-offset)
  (let ((source (get-source-code filename write-date))
        (*readtable* (guess-readtable-for-filename filename)))
    (with-debootstrapping
      (if form-path
          (source-path-string-position form-path source)
          (or character-offset 0)))))

(defun source-hint-snippet (filename write-date position)
  (let ((source (get-source-code filename write-date)))
    (with-input-from-string (s source)
      (read-snippet s position))))

(defun function-source-location (function &optional name)
  (declare (type function function))
  (let ((location (sb-introspect:find-definition-source function)))
    (make-definition-source-location location :function name)))

(defun safe-function-source-location (fun name)
  (if *debug-definition-finding*
      (function-source-location fun name)
      (handler-case (function-source-location fun name)
        (error (e)
          (list :error (format nil "Error: ~A" e))))))

(defimplementation describe-symbol-for-emacs (symbol)
  "Return a plist describing SYMBOL.
Return NIL if the symbol is unbound."
  (let ((result '()))
    (flet ((doc (kind)
             (or (documentation symbol kind) :not-documented))
           (maybe-push (property value)
             (when value
               (setf result (list* property value result)))))
      (maybe-push
       :variable (multiple-value-bind (kind recorded-p)
		     (sb-int:info :variable :kind symbol)
		   (declare (ignore kind))
		   (if (or (boundp symbol) recorded-p)
		       (doc 'variable))))
      (when (fboundp symbol)
	(maybe-push
	 (cond ((macro-function symbol)     :macro)
	       ((special-operator-p symbol) :special-operator)
	       ((typep (fdefinition symbol) 'generic-function)
                :generic-function)
	       (t :function))
	 (doc 'function)))
      (maybe-push
       :setf (if (or (sb-int:info :setf :inverse symbol)
		     (sb-int:info :setf :expander symbol))
		 (doc 'setf)))
      (maybe-push
       :type (if (sb-int:info :type :kind symbol)
		 (doc 'type)))
      result)))

(defimplementation describe-definition (symbol type)
  (case type
    (:variable
     (describe symbol))
    (:function
     (describe (symbol-function symbol)))
    (:setf
     (describe (or (sb-int:info :setf :inverse symbol)
                   (sb-int:info :setf :expander symbol))))
    (:class
     (describe (find-class symbol)))
    (:type
     (describe (sb-kernel:values-specifier-type symbol)))))
  
#+#.(swank-backend::sbcl-with-xref-p)
(progn
  (defmacro defxref (name)
    `(defimplementation ,name (what)
       (sanitize-xrefs   
        (mapcar #'source-location-for-xref-data
                (,(find-symbol (symbol-name name) "SB-INTROSPECT")
                  what)))))
  (defxref who-calls)
  (defxref who-binds)
  (defxref who-sets)
  (defxref who-references)
  (defxref who-macroexpands))

(defun source-location-for-xref-data (xref-data)
  (let ((name (car xref-data))
        (source-location (cdr xref-data)))
    (list name
          (handler-case (make-definition-source-location source-location
                                                         'function
                                                         name)
            (error (e)
              (list :error (format nil "Error: ~A" e)))))))

(defimplementation list-callers (symbol)
  (let ((fn (fdefinition symbol)))
    (sanitize-xrefs
     (mapcar #'function-dspec (sb-introspect:find-function-callers fn)))))

(defimplementation list-callees (symbol)
  (let ((fn (fdefinition symbol)))
    (sanitize-xrefs
     (mapcar #'function-dspec (sb-introspect:find-function-callees fn)))))

(defun sanitize-xrefs (xrefs)
  (remove-duplicates
   (remove-if (lambda (f)
                (member f (ignored-xref-function-names)))
              (loop for entry in xrefs
                    for name = (car entry)
                    collect (if (and (consp name)
                                     (member (car name)
                                             '(sb-pcl::fast-method
                                               sb-pcl::slow-method
                                               sb-pcl::method)))
                                (cons (cons 'defmethod (cdr name))
                                      (cdr entry))
                                entry))
              :key #'car)
   :test (lambda (a b)
           (and (eq (first a) (first b))
                (equal (second a) (second b))))))

(defun ignored-xref-function-names ()
  #-#.(swank-backend::sbcl-with-new-stepper-p)
  '(nil sb-c::step-form sb-c::step-values)
  #+#.(swank-backend::sbcl-with-new-stepper-p)
  '(nil))

(defun function-dspec (fn)
  "Describe where the function FN was defined.
Return a list of the form (NAME LOCATION)."
  (let ((name (sb-kernel:%fun-name fn)))
    (list name (safe-function-source-location fn name))))

;;; macroexpansion

(defimplementation macroexpand-all (form)
  (let ((sb-walker:*walk-form-expand-macros-p* t))
    (sb-walker:walk-form form)))


;;; Debugging

(defvar *sldb-stack-top*)

(defun make-invoke-debugger-hook (hook)
  #'(lambda (condition old-hook)
      ;; Notice that *INVOKE-DEBUGGER-HOOK* is tried before
      ;; *DEBUGGER-HOOK*, so we have to make sure that the latter gets
      ;; run when it was established locally by a user (i.e. changed meanwhile.)
      (if *debugger-hook*
          (funcall *debugger-hook* condition old-hook)
          (funcall hook condition old-hook))))

(defimplementation install-debugger-globally (function)
  (setq *debugger-hook* function)
  (setq sb-ext:*invoke-debugger-hook* (make-invoke-debugger-hook function)))

(defimplementation condition-extras (condition)
  (cond #+#.(swank-backend::sbcl-with-new-stepper-p)
        ((typep condition 'sb-impl::step-form-condition)
         `((:show-frame-source 0)))
        ((typep condition 'sb-int:reference-condition)
         (let ((refs (sb-int:reference-condition-references condition)))
           (if refs
               `((:references ,(externalize-reference refs))))))))

(defun externalize-reference (ref)
  (etypecase ref
    (null nil)
    (cons (cons (externalize-reference (car ref))
                (externalize-reference (cdr ref))))
    ((or string number) ref)
    (symbol 
     (cond ((eq (symbol-package ref) (symbol-package :test))
            ref)
           (t (symbol-name ref))))))

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (declare (type function debugger-loop-fn))
  (let* ((*sldb-stack-top* (or sb-debug:*stack-top-hint* (sb-di:top-frame)))
         (sb-debug:*stack-top-hint* nil))
    (handler-bind ((sb-di:debug-condition
		    (lambda (condition)
                      (signal (make-condition
                               'sldb-condition
                               :original-condition condition)))))
      (funcall debugger-loop-fn))))

#+#.(swank-backend::sbcl-with-new-stepper-p)
(progn
  (defimplementation activate-stepping (frame)
    (declare (ignore frame))
    (sb-impl::enable-stepping))
  (defimplementation sldb-stepper-condition-p (condition)
    (typep condition 'sb-ext:step-form-condition))
  (defimplementation sldb-step-into ()
    (invoke-restart 'sb-ext:step-into))
  (defimplementation sldb-step-next ()
    (invoke-restart 'sb-ext:step-next))
  (defimplementation sldb-step-out ()
    (invoke-restart 'sb-ext:step-out)))

(defimplementation call-with-debugger-hook (hook fun)
  (let ((*debugger-hook* hook)
        (sb-ext:*invoke-debugger-hook* (make-invoke-debugger-hook hook))
        #+#.(swank-backend::sbcl-with-new-stepper-p)
        (sb-ext:*stepper-hook*
         (lambda (condition)
           (typecase condition
             (sb-ext:step-form-condition
              (let ((sb-debug:*stack-top-hint* (sb-di::find-stepped-frame)))
                (sb-impl::invoke-debugger condition)))))))
    (handler-bind (#+#.(swank-backend::sbcl-with-new-stepper-p)
                   (sb-ext:step-condition #'sb-impl::invoke-stepper))
      (funcall fun))))

(defun nth-frame (index)
  (do ((frame *sldb-stack-top* (sb-di:frame-down frame))
       (i index (1- i)))
      ((zerop i) frame)))

(defimplementation compute-backtrace (start end)
  "Return a list of frames starting with frame number START and
continuing to frame number END or, if END is nil, the last frame on the
stack."
  (let ((end (or end most-positive-fixnum)))
    (loop for f = (nth-frame start) then (sb-di:frame-down f)
	  for i from start below end
	  while f collect (make-swank-frame
                           :%frame f
                           :restartable (frame-restartable-p f)))))

(defimplementation print-swank-frame (swank-frame stream)
  (sb-debug::print-frame-call (swank-frame.%frame swank-frame) stream))

(defun frame-restartable-p (frame)
  #+#.(swank-backend::sbcl-with-restart-frame)
  (sb-debug:frame-has-debug-tag-p frame))

;;;; Code-location -> source-location translation

;;; If debug-block info is avaibale, we determine the file position of
;;; the source-path for a code-location.  If the code was compiled
;;; with C-c C-c, we have to search the position in the source string.
;;; If there's no debug-block info, we return the (less precise)
;;; source-location of the corresponding function.

(defun code-location-source-location (code-location)
  (let* ((dsource (sb-di:code-location-debug-source code-location))
         (plist (sb-c::debug-source-plist dsource)))
    (if (getf plist :emacs-buffer)
        (emacs-buffer-source-location code-location plist)
        #+#.(swank-backend::sbcl-with-symbol 'debug-source-from 'sb-di)
        (ecase (sb-di:debug-source-from dsource)
          (:file (file-source-location code-location))
          (:lisp (lisp-source-location code-location)))
        #-#.(swank-backend::sbcl-with-symbol 'debug-source-from 'sb-di)
        (if (sb-di:debug-source-namestring dsource)
            (file-source-location code-location)
            (lisp-source-location code-location)))))

;;; FIXME: The naming policy of source-location functions is a bit
;;; fuzzy: we have FUNCTION-SOURCE-LOCATION which returns the
;;; source-location for a function, and we also have FILE-SOURCE-LOCATION &co
;;; which returns the source location for a _code-location_.
;;;
;;; Maybe these should be named code-location-file-source-location,
;;; etc, turned into generic functions, or something. In the very
;;; least the names should indicate the main entry point vs. helper
;;; status.

(defun file-source-location (code-location)
  (if (code-location-has-debug-block-info-p code-location)
      (source-file-source-location code-location)
      (fallback-source-location code-location)))

(defun fallback-source-location (code-location)
  (let ((fun (code-location-debug-fun-fun code-location)))
    (cond (fun (function-source-location fun))
          (t (error "Cannot find source location for: ~A " code-location)))))

(defun lisp-source-location (code-location)
  (let ((source (prin1-to-string
                 (sb-debug::code-location-source-form code-location 100))))
    (make-location `(:source-form ,source) '(:position 1))))

(defun emacs-buffer-source-location (code-location plist)
  (if (code-location-has-debug-block-info-p code-location)
      (destructuring-bind (&key emacs-buffer emacs-position emacs-string
                                &allow-other-keys)
          plist
        (let* ((pos (string-source-position code-location emacs-string))
               (snipped (with-input-from-string (s emacs-string)
                          (read-snippet s pos))))
          (make-location `(:buffer ,emacs-buffer)
                         `(:offset ,emacs-position ,pos)
                         `(:snippet ,snipped))))
      (fallback-source-location code-location)))

(defun source-file-source-location (code-location)
  (let* ((code-date (code-location-debug-source-created code-location))
         (filename (code-location-debug-source-name code-location))
         (*readtable* (guess-readtable-for-filename filename))
         (source-code (get-source-code filename code-date)))
    (with-debootstrapping
      (with-input-from-string (s source-code)
        (let* ((pos (stream-source-position code-location s))
               (snippet (read-snippet s pos)))
          (make-location `(:file ,filename)
                         `(:position ,pos)
                         `(:snippet ,snippet)))))))

(defun code-location-debug-source-name (code-location)
  (namestring (truename (#+#.(swank-backend::sbcl-with-symbol
                              'debug-source-name 'sb-di)
                             sb-c::debug-source-name
                             #-#.(swank-backend::sbcl-with-symbol
                                  'debug-source-name 'sb-di)
                             sb-c::debug-source-namestring
                         (sb-di::code-location-debug-source code-location)))))

(defun code-location-debug-source-created (code-location)
  (sb-c::debug-source-created
   (sb-di::code-location-debug-source code-location)))

(defun code-location-debug-fun-fun (code-location)
  (sb-di:debug-fun-fun (sb-di:code-location-debug-fun code-location)))

(defun code-location-has-debug-block-info-p (code-location)
  (handler-case
      (progn (sb-di:code-location-debug-block code-location)
             t)
    (sb-di:no-debug-blocks  () nil)))

(defun stream-source-position (code-location stream)
  (let* ((cloc (sb-debug::maybe-block-start-location code-location))
	 (tlf-number (sb-di::code-location-toplevel-form-offset cloc))
	 (form-number (sb-di::code-location-form-number cloc)))
    (multiple-value-bind (tlf pos-map) (read-source-form tlf-number stream)
      (let* ((path-table (sb-di::form-number-translations tlf 0))
             (path (cond ((<= (length path-table) form-number)
                          (warn "inconsistent form-number-translations")
                          (list 0))
                         (t
                          (reverse (cdr (aref path-table form-number)))))))
        (source-path-source-position path tlf pos-map)))))

(defun string-source-position (code-location string)
  (with-input-from-string (s string)
    (stream-source-position code-location s)))

;;; source-path-file-position and friends are in swank-source-path-parser

(defun safe-source-location-for-emacs (code-location)
  (if *debug-definition-finding*
      (code-location-source-location code-location)
      (handler-case (code-location-source-location code-location)
        (error (c) (list :error (format nil "~A" c))))))

(defimplementation frame-source-location-for-emacs (index)
  (safe-source-location-for-emacs
   (sb-di:frame-code-location (nth-frame index))))

(defun frame-debug-vars (frame)
  "Return a vector of debug-variables in frame."
  (sb-di::debug-fun-debug-vars (sb-di:frame-debug-fun frame)))

(defun debug-var-value (var frame location)
  (ecase (sb-di:debug-var-validity var location)
    (:valid (sb-di:debug-var-value var frame))
    ((:invalid :unknown) ':<not-available>)))

(defimplementation frame-locals (index)
  (let* ((frame (nth-frame index))
	 (loc (sb-di:frame-code-location frame))
	 (vars (frame-debug-vars frame)))
    (loop for v across vars collect
          (list :name (sb-di:debug-var-symbol v)
                :id (sb-di:debug-var-id v)
                :value (debug-var-value v frame loc)))))

(defimplementation frame-var-value (frame var)
  (let* ((frame (nth-frame frame))
         (dvar (aref (frame-debug-vars frame) var)))
    (debug-var-value dvar frame (sb-di:frame-code-location frame))))

(defimplementation frame-catch-tags (index)
  (mapcar #'car (sb-di:frame-catches (nth-frame index))))

(defimplementation eval-in-frame (form index)
  (let ((frame (nth-frame index)))
    (funcall (the function
               (sb-di:preprocess-for-eval form
                                          (sb-di:frame-code-location frame)))
             frame)))

#+#.(swank-backend::sbcl-with-restart-frame)
(progn
  (defimplementation return-from-frame (index form)
    (let* ((frame (nth-frame index)))
      (cond ((sb-debug:frame-has-debug-tag-p frame)
             (let ((values (multiple-value-list (eval-in-frame form index))))
               (sb-debug:unwind-to-frame-and-call frame
                                                   (lambda ()
                                                     (values-list values)))))
            (t (format nil "Cannot return from frame: ~S" frame)))))
  
  (defimplementation restart-frame (index)
    (let* ((frame (nth-frame index)))
      (cond ((sb-debug:frame-has-debug-tag-p frame)
             (let* ((call-list (sb-debug::frame-call-as-list frame))
                    (fun (fdefinition (car call-list)))
                    (thunk (lambda () 
                             ;; Ensure that the thunk gets tail-call-optimized
                             (declare (optimize (debug 1)))
                             (apply fun (cdr call-list)))))
               (sb-debug:unwind-to-frame-and-call frame thunk)))
            (t (format nil "Cannot restart frame: ~S" frame))))))

;; FIXME: this implementation doesn't unwind the stack before
;; re-invoking the function, but it's better than no implementation at
;; all.
#-#.(swank-backend::sbcl-with-restart-frame)
(progn
  (defun sb-debug-catch-tag-p (tag)
    (and (symbolp tag)
         (not (symbol-package tag))
         (string= tag :sb-debug-catch-tag)))
  
  (defimplementation return-from-frame (index form)
    (let* ((frame (nth-frame index))
           (probe (assoc-if #'sb-debug-catch-tag-p
                            (sb-di::frame-catches frame))))
      (cond (probe (throw (car probe) (eval-in-frame form index)))
            (t (format nil "Cannot return from frame: ~S" frame)))))
  
  (defimplementation restart-frame (index)
    (let ((frame (nth-frame index)))
      (return-from-frame index (sb-debug::frame-call-as-list frame)))))

;;;;; reference-conditions

(defimplementation format-sldb-condition (condition)
  (let ((sb-int:*print-condition-references* nil))
    (princ-to-string condition)))


;;;; Profiling

(defimplementation profile (fname)
  (when fname (eval `(sb-profile:profile ,fname))))

(defimplementation unprofile (fname)
  (when fname (eval `(sb-profile:unprofile ,fname))))

(defimplementation unprofile-all ()
  (sb-profile:unprofile)
  "All functions unprofiled.")

(defimplementation profile-report ()
  (sb-profile:report))

(defimplementation profile-reset ()
  (sb-profile:reset)
  "Reset profiling counters.")

(defimplementation profiled-functions ()
  (sb-profile:profile))

(defimplementation profile-package (package callers methods)
  (declare (ignore callers methods))
  (eval `(sb-profile:profile ,(package-name (find-package package)))))


;;;; Inspector

(defmethod emacs-inspect ((o t))
  (cond ((sb-di::indirect-value-cell-p o)
         (label-value-line* (:value (sb-kernel:value-cell-ref o))))
	(t
	 (multiple-value-bind (text label parts) (sb-impl::inspected-parts o)
           (list* (format nil "~a~%" text)
                  (if label
                      (loop for (l . v) in parts
                            append (label-value-line l v))
                      (loop for value in parts  for i from 0
                            append (label-value-line i value))))))))

(defmethod emacs-inspect ((o function))
  (let ((header (sb-kernel:widetag-of o)))
    (cond ((= header sb-vm:simple-fun-header-widetag)
                   (label-value-line*
                    (:name (sb-kernel:%simple-fun-name o))
                    (:arglist (sb-kernel:%simple-fun-arglist o))
                    (:self (sb-kernel:%simple-fun-self o))
                    (:next (sb-kernel:%simple-fun-next o))
                    (:type (sb-kernel:%simple-fun-type o))
                    (:code (sb-kernel:fun-code-header o))))
	  ((= header sb-vm:closure-header-widetag)
                   (append
                    (label-value-line :function (sb-kernel:%closure-fun o))
                    `("Closed over values:" (:newline))
                    (loop for i below (1- (sb-kernel:get-closure-length o))
                          append (label-value-line
                                  i (sb-kernel:%closure-index-ref o i)))))
	  (t (call-next-method o)))))

(defmethod emacs-inspect ((o sb-kernel:code-component))
          (append
           (label-value-line*
            (:code-size (sb-kernel:%code-code-size o))
            (:entry-points (sb-kernel:%code-entry-points o))
            (:debug-info (sb-kernel:%code-debug-info o))
            (:trace-table-offset (sb-kernel:code-header-ref
                                  o sb-vm:code-trace-table-offset-slot)))
           `("Constants:" (:newline))
           (loop for i from sb-vm:code-constants-offset
                 below (sb-kernel:get-header-data o)
                 append (label-value-line i (sb-kernel:code-header-ref o i)))
           `("Code:" (:newline)
             , (with-output-to-string (s)
                 (cond ((sb-kernel:%code-debug-info o)
                        (sb-disassem:disassemble-code-component o :stream s))
                       (t
                        (sb-disassem:disassemble-memory
                         (sb-disassem::align
                          (+ (logandc2 (sb-kernel:get-lisp-obj-address o)
                                       sb-vm:lowtag-mask)
                             (* sb-vm:code-constants-offset
                                sb-vm:n-word-bytes))
                          (ash 1 sb-vm:n-lowtag-bits))
                         (ash (sb-kernel:%code-code-size o) sb-vm:word-shift)
                         :stream s)))))))

(defmethod emacs-inspect ((o sb-ext:weak-pointer))
          (label-value-line*
           (:value (sb-ext:weak-pointer-value o))))

(defmethod emacs-inspect ((o sb-kernel:fdefn))
          (label-value-line*
           (:name (sb-kernel:fdefn-name o))
           (:function (sb-kernel:fdefn-fun o))))

(defmethod emacs-inspect :around ((o generic-function))
            (append
             (call-next-method)
             (label-value-line*
              (:pretty-arglist (sb-pcl::generic-function-pretty-arglist o))
              (:initial-methods (sb-pcl::generic-function-initial-methods o))
              )))


;;;; Multiprocessing

#+(and sb-thread
       #.(cl:if (cl:find-symbol "THREAD-NAME" "SB-THREAD") '(and) '(or)))
(progn
  (defvar *thread-id-counter* 0)

  (defvar *thread-id-counter-lock*
    (sb-thread:make-mutex :name "thread id counter lock"))

  (defun next-thread-id ()
    (sb-thread:with-mutex (*thread-id-counter-lock*)
      (incf *thread-id-counter*)))

  (defparameter *thread-id-map* (make-hash-table))

  ;; This should be a thread -> id map but as weak keys are not
  ;; supported it is id -> map instead.
  (defvar *thread-id-map-lock*
    (sb-thread:make-mutex :name "thread id map lock"))

  (defimplementation spawn (fn &key name)
    (sb-thread:make-thread fn :name name))

  (defimplementation thread-id (thread)
    (block thread-id
      (sb-thread:with-mutex (*thread-id-map-lock*)
        (loop for id being the hash-key in *thread-id-map*
              using (hash-value thread-pointer)
              do
              (let ((maybe-thread (sb-ext:weak-pointer-value thread-pointer)))
                (cond ((null maybe-thread)
                       ;; the value is gc'd, remove it manually
                       (remhash id *thread-id-map*))
                      ((eq thread maybe-thread)
                       (return-from thread-id id)))))
        ;; lazy numbering
        (let ((id (next-thread-id)))
          (setf (gethash id *thread-id-map*) (sb-ext:make-weak-pointer thread))
          id))))

  (defimplementation find-thread (id)
    (sb-thread:with-mutex (*thread-id-map-lock*)
      (let ((thread-pointer (gethash id *thread-id-map*)))
        (if thread-pointer
            (let ((maybe-thread (sb-ext:weak-pointer-value thread-pointer)))
              (if maybe-thread
                  maybe-thread
                  ;; the value is gc'd, remove it manually
                  (progn
                    (remhash id *thread-id-map*)
                    nil)))
            nil))))

  (defimplementation thread-name (thread)
    ;; sometimes the name is not a string (e.g. NIL)
    (princ-to-string (sb-thread:thread-name thread)))

  (defimplementation thread-status (thread)
    (if (sb-thread:thread-alive-p thread)
        "RUNNING"
        "STOPPED"))
  #+#.(swank-backend::sbcl-with-weak-hash-tables)
  (progn
    (defparameter *thread-description-map*
      (make-weak-key-hash-table))

    (defvar *thread-descr-map-lock*
      (sb-thread:make-mutex :name "thread description map lock"))
    
    (defimplementation thread-description (thread)
      (sb-thread:with-mutex (*thread-descr-map-lock*)
        (or (gethash thread *thread-description-map*)
            (short-backtrace thread 6 10))))

    (defimplementation set-thread-description (thread description)
      (sb-thread:with-mutex (*thread-descr-map-lock*)
        (setf (gethash thread *thread-description-map*) description)))

    (defun short-backtrace (thread start count)
      (let ((self (current-thread))
            (tag (get-internal-real-time)))
        (sb-thread:interrupt-thread
         thread
         (lambda ()
           (let* ((frames (nthcdr start (sb-debug:backtrace-as-list count))))
             (send self (cons tag frames)))))
        (handler-case
            (sb-ext:with-timeout 0.1
              (let ((frames (cdr (receive-if (lambda (msg) 
                                               (eq (car msg) tag)))))
                    (*print-pretty* nil))
                (format nil "~{~a~^ <- ~}" (mapcar #'car frames))))
          (sb-ext:timeout () ""))))

    )

  (defimplementation make-lock (&key name)
    (sb-thread:make-mutex :name name))

  (defimplementation call-with-lock-held (lock function)
    (declare (type function function))
    (sb-thread:with-recursive-lock (lock) (funcall function)))

  (defimplementation current-thread ()
    sb-thread:*current-thread*)

  (defimplementation all-threads ()
    (sb-thread:list-all-threads))

  (defimplementation interrupt-thread (thread fn)
    (sb-thread:interrupt-thread thread fn))

  (defimplementation kill-thread (thread)
    (sb-thread:terminate-thread thread))

  (defimplementation thread-alive-p (thread)
    (sb-thread:thread-alive-p thread))

  (defvar *mailbox-lock* (sb-thread:make-mutex :name "mailbox lock"))
  (defvar *mailboxes* (list))
  (declaim (type list *mailboxes*))

  (defstruct (mailbox (:conc-name mailbox.))
    thread
    (mutex (sb-thread:make-mutex))
    (waitqueue  (sb-thread:make-waitqueue))
    (queue '() :type list))

  (defun mailbox (thread)
    "Return THREAD's mailbox."
    (sb-thread:with-mutex (*mailbox-lock*)
      (or (find thread *mailboxes* :key #'mailbox.thread)
          (let ((mb (make-mailbox :thread thread)))
            (push mb *mailboxes*)
            mb))))

  (defimplementation send (thread message)
    (let* ((mbox (mailbox thread))
           (mutex (mailbox.mutex mbox)))
      (sb-thread:with-mutex (mutex)
        (setf (mailbox.queue mbox)
              (nconc (mailbox.queue mbox) (list message)))
        (sb-thread:condition-broadcast (mailbox.waitqueue mbox)))))

  (defimplementation receive-if (test &optional timeout)
    (let* ((mbox (mailbox (current-thread)))
           (mutex (mailbox.mutex mbox)))
      (assert (or (not timeout) (eq timeout t)))
      (loop
       (check-slime-interrupts)
       (sb-thread:with-mutex (mutex)
         (let* ((q (mailbox.queue mbox))
                (tail (member-if test q)))
           (when tail 
             (setf (mailbox.queue mbox) (nconc (ldiff q tail) (cdr tail)))
             (return (car tail))))
         (when (eq timeout t) (return (values nil t)))
         ;; FIXME: with-timeout doesn't work properly on Darwin
         #+linux
         (handler-case (sb-ext:with-timeout 0.2
                         (sb-thread:condition-wait (mailbox.waitqueue mbox)
                                                   mutex))
           (sb-ext:timeout ()))
         #-linux  
         (sb-thread:condition-wait (mailbox.waitqueue mbox)
                                   mutex)))))
  )

(defimplementation quit-lisp ()
  #+sb-thread
  (dolist (thread (remove (current-thread) (all-threads)))
    (ignore-errors (sb-thread:interrupt-thread
                    thread (lambda () (sb-ext:quit :recklessly-p t)))))
  (sb-ext:quit))



;;Trace implementations
;;In SBCL, we have:
;; (trace <name>)
;; (trace :methods '<name>) ;to trace all methods of the gf <name>
;; (trace (method <name> <qualifier>? (<specializer>+)))
;; <name> can be a normal name or a (setf name)

(defun toggle-trace-aux (fspec &rest args)
  (cond ((member fspec (eval '(trace)) :test #'equal)
         (eval `(untrace ,fspec))
         (format nil "~S is now untraced." fspec))
        (t
         (eval `(trace ,@(if args `(:encapsulate nil) (list)) ,fspec ,@args))
         (format nil "~S is now traced." fspec))))

(defun process-fspec (fspec)
  (cond ((consp fspec)
         (ecase (first fspec)
           ((:defun :defgeneric) (second fspec))
           ((:defmethod) `(method ,@(rest fspec)))
           ((:labels) `(labels ,(process-fspec (second fspec)) ,(third fspec)))
           ((:flet) `(flet ,(process-fspec (second fspec)) ,(third fspec)))))
        (t
         fspec)))

(defimplementation toggle-trace (spec)
  (ecase (car spec)
    ((setf)
     (toggle-trace-aux spec))
    ((:defmethod)
     (toggle-trace-aux `(sb-pcl::fast-method ,@(rest (process-fspec spec)))))
    ((:defgeneric)
     (toggle-trace-aux (second spec) :methods t))
    ((:call)
     (destructuring-bind (caller callee) (cdr spec)
       (toggle-trace-aux callee :wherein (list (process-fspec caller)))))))

;;; Weak datastructures

(defimplementation make-weak-key-hash-table (&rest args)  
  #+#.(swank-backend::sbcl-with-weak-hash-tables)
  (apply #'make-hash-table :weakness :key args)
  #-#.(swank-backend::sbcl-with-weak-hash-tables)
  (apply #'make-hash-table args))

(defimplementation make-weak-value-hash-table (&rest args)
  #+#.(swank-backend::sbcl-with-weak-hash-tables)
  (apply #'make-hash-table :weakness :value args)
  #-#.(swank-backend::sbcl-with-weak-hash-tables)
  (apply #'make-hash-table args))

(defimplementation hash-table-weakness (hashtable)
  #+#.(swank-backend::sbcl-with-weak-hash-tables)
  (sb-ext:hash-table-weakness hashtable))

#-win32
(defimplementation save-image (filename &optional restart-function)
  (let ((pid (sb-posix:fork)))
    (cond ((= pid 0) 
           (let ((args `(,filename 
                         ,@(if restart-function
                               `((:toplevel ,restart-function))))))
             (apply #'sb-ext:save-lisp-and-die args)))
          (t
           (multiple-value-bind (rpid status) (sb-posix:waitpid pid 0)
             (assert (= pid rpid))
             (assert (and (sb-posix:wifexited status)
                          (zerop (sb-posix:wexitstatus status)))))))))