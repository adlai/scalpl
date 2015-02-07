;;;; actor.lisp

(defpackage #:scalpl.actor
  (:use #:cl #:anaphora #:local-time #:scalpl.util #:chanl)
  (:export #:actor #:perform #:halt #:name #:control #:parent #:child-classes))

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
  ((name :initform (gensym "") :initarg :name :accessor name
         :documentation "Name for identifying this actor and its tasks")
   (tasks :initform nil :documentation "Tasks performing this actor's behavior")
   (control :initform (make-instance 'channel)
            :documentation "Channel for controlling this actor")))

(defgeneric execute (actor command)
  (:method ((actor actor) (command function)) (funcall command actor))
  (:method ((actor actor) (command (eql :halt))) (throw :halt actor)))

(defgeneric enqueue (actor)
  (:method ((actor actor))
    (pexec (:name (format nil "~A~S" (name actor) (now)))
      (catch :halt (perform actor)))))

(defgeneric perform (actor)
  (:documentation "Implement actor's behavior, executing commands by default")
  (:method :before ((actor actor))
    (awhen (recv (slot-value actor 'control) :blockp nil) (execute actor it)))
  (:method :after ((actor actor))
    (with-slots (name tasks) actor
      (setf tasks (cons (enqueue actor)
                        (remove :terminated tasks :key #'task-status))))))

(defgeneric ensure-running (actor)
  (:method ((actor actor))
    (with-slots (name tasks) actor
      (unless (find :alive tasks :key #'task-status)
        (push (enqueue actor) tasks)))))

(defgeneric halt (actor)
  (:documentation "Signals `actor' to terminate")
  (:method ((actor actor)) (send (slot-value actor 'control) :halt)))

(defmethod shared-initialize :after ((actor actor) (slot-names t) &key)
  (ensure-running actor))

;;;
;;; Parent
;;;

;;; An actor with a single child - simple enough to do with mopless ansi clos

(defclass parent (actor) ((child-classes :allocation :class)))

(defgeneric child-initargs (parent child initargs)
  (:method-combination append)
  (:method append ((parent parent) (child t) (initargs t))))

(defmethod shared-initialize :after ((parent parent) (names t) &rest initargs)
  (loop for (child class) on (slot-value parent 'child-classes) by #'cddr
     do (let ((initargs (child-initargs parent child initargs)))
          (if (slot-boundp parent child)
              (apply #'reinitialize-instance (slot-value parent child) initargs)
              (setf (slot-value parent child)
                    (apply #'make-instance class initargs))))))

(defun map-children (parent function)
  (loop for (slot class) on (slot-value parent 'child-classes)
     collect (funcall function (slot-value parent slot))))
