;;;; actor.lisp

(defpackage #:scalpl.actor
  (:use #:cl #:anaphora #:local-time #:scalpl.util #:chanl)
  (:export #:actor #:perform #:halt #:name #:control #:execute #:abbrev #:tasks
           #:ensure-running #:parent #:children #:adopt #:christen #:delegates))

(in-package #:scalpl.actor)

;;;
;;; Actor
;;;

;;; TODO: Incorporate weak references and finalizers into the whole CSPSM model
;;; so they get garbage collected when there are no more references to the
;;; output channels

;;; actor afterthought - maybe actors should be explicit, channels just an
;;; implementation detail? "API methods" on the actor object serve as the
;;; client, and state machine on the channel side serves as the server

(defclass actor ()
  ((name :initarg :name :reader name    ; if you must, use (setf slot-value)
         :documentation "Name for identifying this actor and its tasks")
   (abbrev :initform () :allocation :class)
   (tasks :initform nil :documentation "Tasks performing this actor's behavior")
   (delegates :initform nil :initarg :delegates
              :documentation "Other actors for delegating missing slot values")
   (control :initform (make-instance 'channel) :reader control
            :documentation "Channel for controlling this actor")))

(defvar *date+time* (subseq +iso-8601-format+ 2 9))
(defvar *time-format* (subseq +iso-8601-time-format+ 0 5))

(defgeneric christen (actor type)
  (:method ((actor actor) (type (eql 'actor))) (strftime *date+time*))
  (:method ((actor actor) (type (eql 'task)))
    (with-slots (name abbrev) actor (format nil "~A~@[ ~A~]" name abbrev)))
  (:method :around ((actor actor) (type (eql 'task)))
    (strftime `(,@*time-format* #\Space ,(call-next-method)))))

(defmethod slot-unbound ((class t) (actor actor) (slot-name (eql 'name)))
  (setf (slot-value actor 'name) (christen actor 'actor)))

(defmethod print-object ((actor actor) stream)
  (print-unreadable-object (actor stream :type t :identity t)
    (write-string (name actor) stream)))

(defmacro define-delegated-slot-operation (operation return)
  `(defmethod slot-missing ((class t) (object actor) slot-name
                            (operation (eql ',operation)) &optional new-value)
     (declare (ignore new-value))       ; a sufficiently smart compiler...
     (dolist (actor (slot-value object 'delegates) (call-next-method))
       (when (slot-boundp actor slot-name) (return ,return)))))
(define-delegated-slot-operation slot-value (slot-value actor slot-name))

(defgeneric execute (actor command)
  (:method ((actor actor) (command function)) (funcall command actor))
  (:method ((actor actor) (command (eql :halt))) (throw :halt actor)))

(defgeneric enqueue (actor)
  (:method ((actor actor))
    (pexec (:name (christen actor 'task)
            :initial-bindings '((*read-default-float-format* double-float)))
      (catch :halt (perform actor)))))

(defgeneric perform (actor)
  (:documentation "Implement actor's behavior, executing commands by default")
  (:method :before ((actor actor))
    (with-slots (tasks) actor
      (setf tasks (remove :terminated tasks :key #'task-status))))
  (:method ((actor actor)) (awhen (recv (control actor)) (execute actor it)))
  (:method :after ((actor actor))
    (push (enqueue actor) (slot-value actor 'tasks))))

(defgeneric ensure-running (actor)
  (:method ((actor actor) &aux (cache (slot-value actor 'tasks)))
    (with-slots (tasks) actor           ; this cache business, blech… scheduler?
      (aif (and (find :alive cache :key #'task-status) (eq tasks cache))
           it (aprog1 (enqueue actor) (push it tasks))))))

(defgeneric halt (actor)
  (:documentation "Signals `actor' to terminate")
  (:method ((actor actor)) (send (control actor) :halt)))

(defmethod shared-initialize :after ((actor actor) (slot-names t) &key)
  (ensure-running actor))

;;;
;;; Parent
;;;

(defclass parent (actor) ((children :initform nil)))

(define-delegated-slot-operation slot-boundp t)

(defun map-children (parent function)   ; ... i'm not sure what i expected
  (mapcar function (slot-value parent 'children)))

(defmethod ensure-running :after ((parent parent))
  (map-children parent #'ensure-running))

(defgeneric adopt (parent child)
  (:method ((parent parent) (child actor))
    (pushnew child  (slot-value parent 'children))))

(defgeneric disown (parent child)
  (:method ((parent parent) (child actor))
    (with-slots (children) parent (setf children (remove child children)))))

;;;
;;; Method Combination
;;;

(define-method-combination select ()    ; TODO: &optional sleep
  ((recv (recv . *)) (send (send . *)) (default ())
   (before (:before)) (after (:after)) (around (:around))) (:arguments actor)
  (flet ((build-recv (method &aux (qualifiers (method-qualifiers method)))
           `((recv (slot-value ,actor ',(second qualifiers)) value)
             (call-method (make-method (call-next-method ,actor value))
                          ,method)))
         (build-send (method &aux (qualifiers (method-qualifiers method)))
           `((send (slot-value ,actor ',(second qualifiers))
                   (slot-value ,actor ',(third qualifiers)))
             (call-method ,method))))
    (method-combination-utilities:wrap-primary-form
     `(select ,@(mapcar #'build-recv recv) ,@(mapcar #'build-send send)
              ,@(when default `((t (call-method ,(first default))))))
     around before after)))

(defgeneric performance (actor &optional value)
  (:documentation "`select'-based `perform'") (:method-combination select)
  (:method ((actor actor) &optional ignore) (declare (ignore ignore)) (sleep 1))
  (:method recv control ((actor actor) &optional op) (execute actor op))
  (:method :before ((actor actor) &optional ignore) (declare (ignore ignore))
    (with-slots (tasks) actor
      (setf tasks (remove :terminated tasks :key #'task-status))))
  (:method :after ((actor actor) &optional ignore) (declare (ignore ignore))
    (push (enqueue actor) (slot-value actor 'tasks))))
