;;;; actor.lisp

(defpackage #:scalpl.actor
  (:use #:cl #:anaphora #:local-time #:scalpl.util #:chanl)
  (:export #:actor #:perform #:halt #:name #:control #:execute
           #:parent #:children #:adopt #:disown #:christen))

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
   (tasks :initform nil :documentation "Tasks performing this actor's behavior")
   (delegates :initform nil :initarg :delegates
              :documentation "Other actors for delegating missing slot values")
   (control :initform (make-instance 'channel) :reader control
            :documentation "Channel for controlling this actor")))

(defvar *date+time* (subseq +iso-8601-format+ 2 9))
(defvar *time-format* (subseq +iso-8601-time-format+ 0 5))

(defgeneric christen (actor type)
  (:method ((actor actor) (type (eql 'actor))) (strftime *date+time*))
  (:method ((actor actor) (type (eql 'task))) (name actor))
  (:method :around ((actor actor) (type (eql 'task)))
    (strftime `(,@*time-format* #\Space ,(call-next-method)))))

(defmethod slot-unbound ((class t) (actor actor) (slot-name (eql 'name)))
  (setf (slot-value actor 'name) (christen actor 'actor)))

(defmethod print-object ((actor actor) stream)
  (print-unreadable-object (actor stream :type t :identity t)
    (write-string (name actor) stream)))

(defmethod slot-missing ((class t) (object actor) slot-name
                         (operation (eql 'slot-value)) &optional new-value)
  (declare (ignore new-value))          ; a sufficiently smart compiler...
  (dolist (actor (slot-value object 'delegates) (call-next-method))
    (when (slot-boundp actor slot-name) (return (slot-value actor slot-name)))))

(defgeneric execute (actor command)
  (:method ((actor actor) (command function)) (funcall command actor))
  (:method ((actor actor) (command (eql :halt))) (throw :halt actor)))

(defgeneric enqueue (actor)
  (:method ((actor actor))
    (pexec (:name (christen actor 'task)) (catch :halt (perform actor)))))

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
    (with-slots (tasks) actor
      (unless (and (find :alive cache :key #'task-status) (eq tasks cache))
        (push (enqueue actor) tasks)))))

(defgeneric halt (actor)
  (:documentation "Signals `actor' to terminate")
  (:method ((actor actor)) (send (control actor) :halt)))

(defmethod shared-initialize :after ((actor actor) (slot-names t) &key)
  (ensure-running actor))

;;;
;;; Parent
;;;

(defclass parent (actor) ((children :initform nil)))

(defun map-children (parent function)   ; ... i'm not sure what i expected
  (mapcar function (slot-value parent 'children)))

(defmethod ensure-running :after ((parent parent))
  (map-children parent #'ensure-running))

(defgeneric adopt (parent child)
  (:method ((parent parent) (child actor))
    (pushnew child  (slot-value parent 'children))
    (pushnew parent (slot-value child  'delegates))))

(defgeneric disown (parent child)
  (:method ((parent parent) (child actor))
    (with-slots (children) parent (setf children  (remove child  children)))
    (with-slots (delegates) child (setf delegates (remove parent delegates)))))
