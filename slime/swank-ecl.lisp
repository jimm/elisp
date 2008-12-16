;;;; -*- indent-tabs-mode: nil -*-
;;;
;;; swank-ecl.lisp --- SLIME backend for ECL.
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

;;; Administrivia

(in-package :swank-backend)

(defvar *tmp*)

(eval-when (:compile-toplevel :load-toplevel :execute)
(if (find-package :gray)
  (import-from :gray *gray-stream-symbols* :swank-backend)
  (import-from :ext *gray-stream-symbols* :swank-backend))

(swank-backend::import-swank-mop-symbols :clos
 '(:eql-specializer
   :eql-specializer-object
   :generic-function-declarations
   :specializer-direct-methods
   :compute-applicable-methods-using-classes)))


;;;; TCP Server

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sockets))

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
  (sb-bsd-sockets:socket-close socket))

(defimplementation accept-connection (socket
                                      &key external-format
                                      buffering timeout)
  (declare (ignore buffering timeout external-format))
  (make-socket-io-stream (accept socket)))

(defun make-socket-io-stream (socket)
  (sb-bsd-sockets:socket-make-stream socket
                                     :output t
                                     :input t
                                     :element-type 'base-char))

(defun accept (socket)
  "Like socket-accept, but retry on EAGAIN."
  (loop (handler-case
            (return (sb-bsd-sockets:socket-accept socket))
          (sb-bsd-sockets:interrupted-error ()))))

(defimplementation preferred-communication-style ()
  (values nil))


;;;; Unix signals

(defimplementation install-sigint-handler (handler)
  (let ((old-handler (symbol-function 'si:terminal-interrupt)))
    (setf (symbol-function 'si:terminal-interrupt)
          (if (consp handler)
              (car handler)
              (lambda (&rest args)
                (declare (ignore args))
                (funcall handler)
                (continue))))
    (list old-handler)))


(defimplementation getpid ()
  (si:getpid))

#+nil
(defimplementation set-default-directory (directory)
  (ext::chdir (namestring directory))
  ;; Setting *default-pathname-defaults* to an absolute directory
  ;; makes the behavior of MERGE-PATHNAMES a bit more intuitive.
  (setf *default-pathname-defaults* (ext::getcwd))
  (default-directory))

#+nil
(defimplementation default-directory ()
  (namestring (ext:getcwd)))

(defimplementation quit-lisp ()
  (ext:quit))


;;;; Compilation

(defvar *buffer-name* nil)
(defvar *buffer-start-position*)
(defvar *buffer-string*)
(defvar *compile-filename*)

(defun signal-compiler-condition (&rest args)
  (signal (apply #'make-condition 'compiler-condition args)))

(defun handle-compiler-warning (condition)
  (signal-compiler-condition
   :original-condition condition
   :message (format nil "~A" condition)
   :severity :warning
   :location
   (if *buffer-name*
       (make-location (list :buffer *buffer-name*)
                      (list :offset *buffer-start-position* 0))
       ;; ;; compiler::*current-form*
       ;; (if compiler::*current-function*
       ;;     (make-location (list :file *compile-filename*)
       ;;                    (list :function-name   
       ;;                          (symbol-name
       ;;                           (slot-value compiler::*current-function*
       ;;                                       'compiler::name))))
       (list :error "No location found.")
           ;; )
       )))

(defimplementation call-with-compilation-hooks (function)
  (handler-bind ((warning #'handle-compiler-warning))
    (funcall function)))

(defimplementation swank-compile-file (*compile-filename* load-p
                                       external-format)
  (declare (ignore external-format))
  (with-compilation-hooks ()
    (let ((*buffer-name* nil))
      (multiple-value-bind (fn warn fail) 
          (compile-file *compile-filename*)
        (when load-p (unless fail (load fn)))))))

(defimplementation swank-compile-string (string &key buffer position directory
                                                debug)
  (declare (ignore directory debug))
  (with-compilation-hooks ()
    (let ((*buffer-name* buffer)
          (*buffer-start-position* position)
          (*buffer-string* string))
      (with-input-from-string (s string)
        (compile-from-stream s :load t)))))

(defun compile-from-stream (stream &rest args)
  (let ((file (si::mkstemp "TMP:ECLXXXXXX")))
    (with-open-file (s file :direction :output :if-exists :overwrite)
      (do ((line (read-line stream nil) (read-line stream nil)))
	  ((not line))
	(write-line line s)))
    (unwind-protect
         (apply #'compile-file file args)
      (delete-file file))))


;;;; Documentation

(defun grovel-docstring-for-arglist (name type)
  (flet ((compute-arglist-offset (docstring)
           (when docstring
             (let ((pos1 (search "Args: " docstring)))
               (if pos1
                   (+ pos1 6)
                   (let ((pos2 (search "Syntax: " docstring)))
                     (when pos2
                       (+ pos2 8))))))))
    (let* ((docstring (si::get-documentation name type))
           (pos (compute-arglist-offset docstring)))
      (if pos
          (multiple-value-bind (arglist errorp)
              (ignore-errors
                (values (read-from-string docstring t nil :start pos)))
            (if errorp :not-available (cdr arglist)))
          :not-available ))))

(defimplementation arglist (name)
  (cond ((special-operator-p name)
         (grovel-docstring-for-arglist name 'function))
        ((macro-function name)
         (grovel-docstring-for-arglist name 'function))
        ((or (functionp name) (fboundp name))
         (multiple-value-bind (name fndef)
             (if (functionp name)
                 (values (function-name name) name)
                 (values name (fdefinition name)))
           (typecase fndef
             (generic-function
              (clos::generic-function-lambda-list fndef))
             (compiled-function
              (grovel-docstring-for-arglist name 'function))
             (function
              (let ((fle (function-lambda-expression fndef)))
                (case (car fle)
                  (si:lambda-block (caddr fle))
                  (t               :not-available)))))))
        (t :not-available)))

(defimplementation function-name (f)
  (si:compiled-function-name f))

(defimplementation macroexpand-all (form)
  ;;; FIXME! This is not the same as a recursive macroexpansion!
  (macroexpand form))

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (dolist (type '(:VARIABLE :FUNCTION :CLASS))
      (let ((doc (describe-definition symbol type)))
        (when doc
          (setf result (list* type doc result)))))
    result))

(defimplementation describe-definition (name type)
  (case type
    (:variable (documentation name 'variable))
    (:function (documentation name 'function))
    (:class (documentation name 'class))
    (t nil)))

;;; Debugging

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   '(si::*break-env*
     si::*ihs-top*
     si::*ihs-current*
     si::*ihs-base*
     si::*frs-base*
     si::*frs-top*
     si::*tpl-commands*
     si::*tpl-level*
     si::frs-top
     si::ihs-top
     si::ihs-fun
     si::ihs-env
     si::sch-frs-base
     si::set-break-env
     si::set-current-ihs
     si::tpl-commands)))

(defvar *backtrace* '())

(defun in-swank-package-p (x)
  (and
   (symbolp x)
   (member (symbol-package x)
           (list #.(find-package :swank)
                 #.(find-package :swank-backend)
                 #.(ignore-errors (find-package :swank-mop))
                 #.(ignore-errors (find-package :swank-loader))))
   t))

(defun is-swank-source-p (name)
  (setf name (pathname name))
  (pathname-match-p
   name
   (make-pathname :defaults swank-loader::*source-directory*
                  :name (pathname-name name)
                  :type (pathname-type name)
                  :version (pathname-version name))))

(defun is-ignorable-fun-p (x)
  (or
   (in-swank-package-p (frame-name x))
   (multiple-value-bind (file position)
       (ignore-errors (si::bc-file (car x)))
     (declare (ignore position))
     (if file (is-swank-source-p file)))))

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (declare (type function debugger-loop-fn))
  (let* ((*tpl-commands* si::tpl-commands)
         (*ihs-top* (ihs-top 'call-with-debugging-environment))
         (*ihs-current* *ihs-top*)
         (*frs-base* (or (sch-frs-base *frs-top* *ihs-base*) (1+ (frs-top))))
         (*frs-top* (frs-top))
         (*read-suppress* nil)
         (*tpl-level* (1+ *tpl-level*))
         (*backtrace* (loop for ihs from *ihs-base* below *ihs-top*
                            collect (list (si::ihs-fun ihs)
                                          (si::ihs-env ihs)
                                          nil))))
    (loop for f from *frs-base* until *frs-top*
          do (let ((i (- (si::frs-ihs f) *ihs-base* 1)))
               (when (plusp i)
                 (let* ((x (elt *backtrace* i))
                        (name (si::frs-tag f)))
                   (unless (si::fixnump name)
                     (push name (third x)))))))
    (setf *backtrace* (remove-if #'is-ignorable-fun-p (nreverse *backtrace*)))
    (setf *tmp* *backtrace*)
    (set-break-env)
    (set-current-ihs)
    (let ((*ihs-base* *ihs-top*))
      (funcall debugger-loop-fn))))

(defimplementation call-with-debugger-hook (hook fun)
  (let ((*debugger-hook* hook)
        (*ihs-base*(si::ihs-top 'call-with-debugger-hook)))
    (funcall fun)))

(defimplementation compute-backtrace (start end)
  (when (numberp end)
    (setf end (min end (length *backtrace*))))
  (loop for f in (subseq *backtrace* start end)
        collect (make-swank-frame :%frame f :restartable :unknown)))

(defun frame-name (frame)
  (let ((x (first frame)))
    (if (symbolp x)
      x
      (function-name x))))

(defun function-position (fun)
  (multiple-value-bind (file position)
      (si::bc-file fun)
    (and file (make-location `(:file ,file) `(:position ,position)))))

(defun frame-function (frame)
  (let* ((x (first frame))
         fun position)
    (etypecase x
      (symbol (and (fboundp x)
                   (setf fun (fdefinition x)
                         position (function-position fun))))
      (function (setf fun x position (function-position x))))
    (values fun position)))

(defun frame-decode-env (frame)
  (let ((functions '())
        (blocks '())
        (variables '()))
    (dolist (record (second frame))
      (let* ((record0 (car record))
	     (record1 (cdr record)))
	(cond ((symbolp record0)
	       (setq variables (acons record0 record1 variables)))
	      ((not (si::fixnump record0))
	       (push record1 functions))
	      ((symbolp record1)
	       (push record1 blocks))
	      (t
	       ))))
    (values functions blocks variables)))

(defimplementation print-swank-frame (swank-frame stream)
  (let ((frame (swank-frame.%frame swank-frame)))
    (format stream "~A" (first frame))))

(defimplementation frame-source-location-for-emacs (frame-number)
  (nth-value 1 (frame-function (elt *backtrace* frame-number))))

(defimplementation frame-catch-tags (frame-number)
  (third (elt *backtrace* frame-number)))

(defimplementation frame-locals (frame-number)
  (loop for (name . value) in (nth-value 2 (frame-decode-env (elt *backtrace* frame-number)))
        with i = 0
        collect (list :name name :id (prog1 i (incf i)) :value value)))

(defimplementation frame-var-value (frame-number var-id)
  (elt (nth-value 2 (frame-decode-env (elt *backtrace* frame-number)))
       var-id))

(defimplementation disassemble-frame (frame-number)
  (let ((fun (frame-fun (elt *backtrace* frame-number))))
    (disassemble fun)))

(defimplementation eval-in-frame (form frame-number)
  (let ((env (second (elt *backtrace* frame-number))))
    (si:eval-with-env form env)))

;;;; Inspector

(defmethod emacs-inspect ((o t))
  ; ecl clos support leaves some to be desired
  (cond
    ((streamp o)
     (list*
      (format nil "~S is an ordinary stream~%" o)
      (append
       (list
        "Open for "
        (cond
          ((ignore-errors (interactive-stream-p o)) "Interactive")
          ((and (input-stream-p o) (output-stream-p o)) "Input and output")
          ((input-stream-p o) "Input")
          ((output-stream-p o) "Output"))
        `(:newline) `(:newline))
       (label-value-line*
        ("Element type" (stream-element-type o))
        ("External format" (stream-external-format o)))
       (ignore-errors (label-value-line*
                       ("Broadcast streams" (broadcast-stream-streams o))))
       (ignore-errors (label-value-line*
                       ("Concatenated streams" (concatenated-stream-streams o))))
       (ignore-errors (label-value-line*
                       ("Echo input stream" (echo-stream-input-stream o))))
       (ignore-errors (label-value-line*
                       ("Echo output stream" (echo-stream-output-stream o))))
       (ignore-errors (label-value-line*
                       ("Output String" (get-output-stream-string o))))
       (ignore-errors (label-value-line*
                       ("Synonym symbol" (synonym-stream-symbol o))))
       (ignore-errors (label-value-line*
                       ("Input stream" (two-way-stream-input-stream o))))
       (ignore-errors (label-value-line*
                       ("Output stream" (two-way-stream-output-stream o)))))))
    (t
     (let* ((cl (si:instance-class o))
            (slots (clos:class-slots cl)))
       (list* (format nil "~S is an instance of class ~A~%"
                       o (clos::class-name cl))
               (loop for x in slots append
                    (let* ((name (clos:slot-definition-name x))
                           (value (clos::slot-value o name)))
                      (list
                       (format nil "~S: " name)
                       `(:value ,value)
                       `(:newline)))))))))

;;;; Definitions

(defimplementation find-definitions (name)
  (if (fboundp name)
      (let ((tmp (find-source-location (symbol-function name))))
        `(((defun ,name) ,tmp)))))

(defimplementation find-source-location (obj)
  (setf *tmp* obj)
  (or
   (typecase obj
     (function
      (multiple-value-bind (file pos) (ignore-errors (si::bc-file obj))
        (if (and file pos) 
            (make-location
              `(:file ,(namestring file))
              `(:position ,pos)
              `(:snippet
                ,(with-open-file (s file)
                                 (skip-toplevel-forms pos s)
                                 (skip-comments-and-whitespace s)
                                 (read-snippet s))))))))
   `(:error (format nil "Source definition of ~S not found" obj))))

;;;; Threads

#+threads
(progn
  (defvar *thread-id-counter* 0)

  (defvar *thread-id-counter-lock*
    (mp:make-lock :name "thread id counter lock"))

  (defun next-thread-id ()
    (mp:with-lock (*thread-id-counter-lock*)
      (incf *thread-id-counter*)))

  (defparameter *thread-id-map* (make-hash-table))
  (defparameter *id-thread-map* (make-hash-table))

  (defvar *thread-id-map-lock*
    (mp:make-lock :name "thread id map lock"))

  ; ecl doesn't have weak pointers
  (defimplementation spawn (fn &key name)
    (let ((thread (mp:make-process :name name))
	  (id (next-thread-id)))
      (mp:process-preset
	thread
	#'(lambda ()
	    (unwind-protect
	      (mp:with-lock (*thread-id-map-lock*)
	        (setf (gethash id *thread-id-map*) thread)
                (setf (gethash thread *id-thread-map*) id))
	      (funcall fn)
	      (mp:with-lock (*thread-id-map-lock*)
                (remhash thread *id-thread-map*)
                (remhash id *thread-id-map*)))))
      (mp:process-enable thread)))

  (defimplementation thread-id (thread)
    (block thread-id
      (mp:with-lock (*thread-id-map-lock*)
        (or (gethash thread *id-thread-map*)
            (let ((id (next-thread-id)))
              (setf (gethash id *thread-id-map*) thread)
              (setf (gethash thread *id-thread-map*) id)
              id)))))

  (defimplementation find-thread (id)
    (mp:with-lock (*thread-id-map-lock*)
      (gethash id *thread-id-map*)))

  (defimplementation thread-name (thread)
    (mp:process-name thread))

  (defimplementation thread-status (thread)
    (if (mp:process-active-p thread)
        "RUNNING"
        "STOPPED"))

  (defimplementation make-lock (&key name)
    (mp:make-lock :name name))

  (defimplementation call-with-lock-held (lock function)
    (declare (type function function))
    (mp:with-lock (lock) (funcall function)))

  (defimplementation current-thread ()
    mp:*current-process*)

  (defimplementation all-threads ()
    (mp:all-processes))

  (defimplementation interrupt-thread (thread fn)
    (mp:interrupt-process thread fn))

  (defimplementation kill-thread (thread)
    (mp:process-kill thread))

  (defimplementation thread-alive-p (thread)
    (mp:process-active-p thread))

  (defvar *mailbox-lock* (mp:make-lock :name "mailbox lock"))

  (defstruct (mailbox (:conc-name mailbox.))
    (mutex (mp:make-lock :name "process mailbox"))
    (queue '() :type list))

  (defun mailbox (thread)
    "Return THREAD's mailbox."
    (mp:with-lock (*mailbox-lock*)
      (or (find thread *mailboxes* :key #'mailbox.thread)
          (let ((mb (make-mailbox :thread thread)))
            (push mb *mailboxes*)
            mb))))

  (defimplementation send (thread message)
    (let* ((mbox (mailbox thread))
           (mutex (mailbox.mutex mbox)))
      (mp:interrupt-process
	thread
	(lambda ()
	  (mp:with-lock (mutex)
            (setf (mailbox.queue mbox)
              (nconc (mailbox.queue mbox) (list message))))))))

  (defimplementation receive ()
    (block got-mail
      (let* ((mbox (mailbox mp:*current-process*))
             (mutex (mailbox.mutex mbox)))
        (loop
	  (mp:with-lock (mutex)
            (if (mailbox.queue mbox)
	      (return-from got-mail (pop (mailbox.queue mbox)))))
          ;interrupt-process will halt this if it takes longer than 1sec
          (sleep 1)))))

  (defmethod stream-finish-output ((stream stream))
    (finish-output stream))

  )

