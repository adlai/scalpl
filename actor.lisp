;;;; actor.lisp

(defpackage #:scalpl.actor
  (:use #:cl #:scalpl.util))

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
  ((thread :documentation "Thread performing this actor's behavior")
   (control :documentation "Channel for controlling this actor")
   (default :documentation "Default command in absence of control messages"
            :initarg :default :initform (error "Must supply default state"))
   (children :allocation :class :initform nil
             :documentation "Children of this actor")
   (channels :allocation :class :initform nil
             :documentation "Channel slot names")))

(defmethod initialize-instance :before ((actor actor) &key)
  (flet ((initialize (slot)
           (setf (slot-value actor slot)
                 (make-instance 'chanl:channel))))
    (initialize 'control)
    (mapc #'initialize (slot-value actor 'channels))))

(defgeneric perform (actor command)
  (:method ((actor actor) (command function))
    (funcall command actor)))

(defgeneric christen (actor)
  (:documentation "Generates a name for `actor', after slot initialization")
  (:method ((actor actor)) (format nil "actor for ~A" actor)))

(defun kill-actor (actor)
  ;; todo: timeout for chanl:kill ?
  (chanl:send (slot-value actor 'control) nil))

(defmethod reinitialize-instance :before ((actor actor) &key kill)
  (when kill (kill-actor actor)))

(defmethod shared-initialize :after ((actor actor) slot-names &key)
  (when (or (not (slot-boundp actor 'thread))
            (eq :terminated (chanl:task-status (slot-value actor 'thread))))
    (setf (slot-value actor 'thread)
          (chanl:pexec (:name (christen actor)
                        :initial-bindings `((*read-default-float-format* double-float)))
            (perform actor (chanl:recv (slot-value actor 'control)))))))
