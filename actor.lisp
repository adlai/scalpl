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
  ((task :documentation "Task performing this actor's behavior")
   (control :documentation "Channel for controlling this actor")
   (children :allocation :class :initform nil
             :documentation "Children of this actor")
   (channels :allocation :class :initform '(control)
             :documentation "Channel slot names")))

(defgeneric channels (actor)
  (:method-combination append)
  (:method :around ((actor actor))
    (remove-duplicates (call-next-method)))
  (:method append ((actor actor))
    (slot-value actor 'channels)))

(defmethod initialize-instance :before ((actor actor) &key)
  (dolist (slot (channels actor))
    (setf (slot-value actor slot) (make-instance 'channel))))

(defgeneric execute (actor command)
  (:method ((actor actor) (command function)) (funcall command actor)))

(defgeneric perform (actor)
  (:method ((actor actor)) (execute actor (recv (slot-value actor 'control)))))

(defgeneric christen (actor)
  ;; (:method-combination print-unreadable-object :type t :identity t)
  (:method ((actor actor))
    (with-output-to-string (out)
      (print-unreadable-object (actor out :type t :identity t)))))

(defgeneric ensure-running (actor)
  (:method ((actor actor))
    (when (or (not (slot-boundp actor 'task))
              (eq :terminated (task-status (slot-value actor 'task))))
      (setf (slot-value actor 'task)
            (chanl:pexec (:name (christen actor)
                          :initial-bindings `((*read-default-float-format* double-float)))
              (loop (perform actor)))))))

(defgeneric halt (actor)
  (:documentation "Signals `actor' to terminate")
  (:method ((actor actor)) (send (slot-value actor 'control) nil)))

(defmethod reinitialize-instance :before ((actor actor) &key kill)
  (when kill (kill-actor actor)))

(defmethod shared-initialize :after ((actor actor) slot-names &key)
  (ensure-running actor))
