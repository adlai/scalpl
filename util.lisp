(defpackage #:scalpl.util
  (:use #:c2cl #:anaphora #:st-json #:parse-float #:string-case)
  (:export #:once-only
           #:shallow-copy
           #:dbz-guard
           #:slot-reduce
           #:with-json-slots
           #:aand1
           #:awhen1
           #:parse-price
           #:parse-float
           #:string-octets
           #:mapcar-jso
           #:rehome-symbol
           #:rehome-class
           #:string-case
           #:mapjso*
           #:urlencode-params
           ))

(in-package #:scalpl.util)

;;; Actually useful

(defun shallow-copy (object)
  (multiple-value-bind (create init) (make-load-form-saving-slots object)
    (aprog1 (eval create)
      (labels ((walk (form) (cond ((eq form object) it)
                                  ((atom form) form)
                                  (t (cons (car form)
                                           (mapcar #'walk (cdr form)))))))
        (eval (walk init))))))

(defmacro dbz-guard (form) `(handler-case ,form (division-by-zero () 0)))

;;; right now this is only for convenience at the REPL
;;; as was suggested in #lisp, if there's ever a need to do this in the actual
;;; code, it's a red flag that some abstraction is missing
(defmacro slot-reduce (root &rest slots)
  `(reduce 'slot-value ',slots :initial-value ,root))

(define-setf-expander slot-reduce (root &rest slots &environment env)
  (assert (not (null slots)) (slots) "must supply at least one slot")
  (let ((path (butlast slots)) (end (car (last slots))))
    (get-setf-expansion `(slot-value (slot-reduce ,root ,@path) ',end) env)))

(defun rehome-symbol (symbol new-home &aux (old-home (symbol-package symbol)))
  (unintern symbol old-home)
  (import (list symbol) new-home)
  (import (list symbol) old-home))

(defgeneric rehome-class (class new-home)
  (:method ((class symbol) new-home)
    (rehome-class (find-class class) (find-package new-home)))
  (:method ((class class) (new-home package))
    (let ((old-home (symbol-package (class-name class)))
          (symbols (cons (class-name class)
                         (mapcar 'slot-definition-name
                                 (class-direct-slots class)))))
      (mapc (lambda (symbol) (unintern symbol old-home)) symbols)
      (import symbols new-home)
      (import symbols old-home))))

(define-condition price-precision-problem ()
  ((price-string :initarg :price-string)
   (expected     :initarg :expected)
   (delta        :initarg :delta))
  (:report
   (lambda (condition stream)
     (with-slots (price-string expected delta) condition
       (format stream "Price string ~S contains ~D decimals (expected ~D)."
               price-string (+ expected delta) expected)))))

(defun parse-price (price-string decimals)
  (let ((dot (position #\. price-string)))
    (multiple-value-bind (int end) (parse-integer (remove #\. price-string))
      (let ((delta (- end dot decimals)))
        (if (zerop delta) int
            (values (if (> delta 0)
                        (floor int (expt 10 delta))
                        (* int (expt 10 (- delta))))
                    delta))))))

(defun string-octets (string)
  (declare (type string string))
  (map '(simple-array (unsigned-byte 8) (*)) 'char-code string))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym (symbol-name n)))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defmacro with-json-slots ((&rest slot-bindings) object &body body)
  (once-only (object)
    `(symbol-macrolet
         ,(mapcar (lambda (binding)
                    (if (consp binding)
                        (destructuring-bind (var slot) binding
                          `(,var (getjso ,slot ,object)))
                        `(,binding (getjso ,(string-downcase (string binding))
                                           ,object))))
                  slot-bindings)
       ,@body)))

(defun mapcar-jso (thunk jso &aux list)
  (mapjso (lambda (key val)
            (push (funcall thunk key val) list))
          jso)
  list)

(defun mapjso* (thunk jso)
  (mapjso thunk jso)
  jso)

;;; Bastardized shamelessly from #'drakma::alist-to-url-encoded-string
(defun urlencode-params (params)
  (with-output-to-string (out)
    (loop for first = t then nil
       for (name . value) in params
       unless first do (write-char #\& out)
       do (format out "~A~:[~;=~A~]"
                  (drakma:url-encode name drakma::*drakma-default-external-format*)
                  value
                  (drakma:url-encode value drakma::*drakma-default-external-format*)))))

;;; anaphora!

(defmacro when1 (test &body body)
  (once-only (test) `(when ,test ,@body ,test)))

(defmacro awhen1 (test &body body)
  `(anaphora::anaphoric when1 ,test ,@body))

(defmacro and1 (test &rest rest)
  (once-only (test) `(and ,test ,@rest ,test)))

(defmacro aand1 (test &rest rest)
  `(anaphora::anaphoric and1 ,test ,@rest))
