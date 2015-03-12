(defpackage #:scalpl.util
  (:use #:c2cl #:anaphora #:parse-float #:string-case #:local-time)
  (:export #:once-only
           #:shallow-copy
           #:dbz-guard
           #:slot-reduce #:slot-reducer #:slot-setter
           #:aand1
           #:awhen1
           #:parse-price
           #:parse-float
           #:string-octets #:octets
           #:rehome-symbol
           #:rehome-class
           #:string-case
           #:urlencode-params
           #:break-errors
           #:kw #:mvwrap
           #:subseq*
           #:with-aslots
           #:lazy-do-instances
           #:strftime
           ;; json
           #:read-json
           #:getjso
           #:mapjso
           #:mapcar-jso
           #:jso-keys
           #:with-json-slots
           #:mapjso*
           ))

(in-package #:scalpl.util)

;;; Actually useful

(defun strftime (format) (format-timestring () (now) :format format))

(defmacro lazy-do-instances (class agitation &body forms)
  "evaluates FORMS with IT bound to each instance of CLASS touched by AGITATION"
  `(remove-method #'update-instance-for-redefined-class
                  (prog1 (defmethod update-instance-for-redefined-class
                              ((it ,class) add discard plist &rest keys)
                            (assert (every 'null (list add discard plist keys))
                                    () "is your refrigerator running?") ,@forms)
                    (make-instances-obsolete ',class) ,agitation)))

(defun subseq* (sequence start &optional end)
  (handler-case (subseq sequence start end)
    (error () (subseq sequence start))))

(defun kw (thing) (intern (string-upcase (string thing)) :keyword))

(defmacro mvwrap (slot function)
  `(apply 'values (and ,slot `(,,(kw slot) ,(,function ,slot)))))

(defmacro break-errors (typespec &body forms)
  `(ignore-errors (let ((*break-on-signals* ',typespec)) ,@forms)))

(defun shallow-copy (object)
  (multiple-value-bind (create init) (make-load-form-saving-slots object)
    (aprog1 (eval create)
      (labels ((walk (form)
                 (cond ((eq form object) it) ((atom form) form)
                       (t (cons (car form) (mapcar #'walk (cdr form)))))))
        (eval (walk init))))))

(defmacro dbz-guard (form) `(handler-case ,form (division-by-zero () 0)))

;;; right now this is only for convenience at the REPL
;;; as was suggested in #lisp, if there's ever a need to do this in the actual
;;; code, it's a red flag that some abstraction is missing
(defmacro slot-reduce (root &rest slots)
  `(reduce 'slot-value ',slots :initial-value ,root))

(defmacro slot-reducer (&rest slots &aux (root (gensym "root")))
  `(lambda (,root) (slot-reduce ,root ,@slots)))

(defmacro slot-setter (new-value &rest slots &aux (root (gensym "root")))
  `(lambda (,root) (setf (slot-reduce ,root ,@slots) ,new-value)))

(define-setf-expander slot-reduce (root &rest slots &environment env)
  (assert (not (null slots)) (slots) "must supply at least one slot")
  (let ((path (butlast slots)) (end (car (last slots))))
    (get-setf-expansion `(slot-value (slot-reduce ,root ,@path) ',end) env)))

(defun rehome-symbol (symbol new-home &aux (source (symbol-package symbol)))
  (unintern symbol source) (import symbol new-home) (import symbol source))

(defgeneric rehome-class (class new-home)
  (:method ((class symbol) new-home)
    (rehome-class (find-class class) (find-package new-home)))
  (:method ((class class) (new-home package))
    (mapcar (lambda (symbol) (rehome-symbol symbol new-home))
            (cons (class-name class) (mapcar 'slot-definition-name
                                             (class-direct-slots class))))))

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

(deftype octets (&optional (size '*)) `(simple-array (unsigned-byte 8) (,size)))
(declaim (ftype (function (string) octets) string-octets))
(defun string-octets (string) (map 'octets 'char-code string))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym (symbol-name n)))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defun urlencode-params (params)
  (with-output-to-string (out)
    (labels ((urlencode (thing)
               (drakma:url-encode (princ-to-string thing)
                                  drakma::*drakma-default-external-format*))
             (write-param (param)
               (destructuring-bind (name . value) param
                 (format out "~A=~A" (urlencode name) (urlencode value)))))
      (awhen (first params) (write-param it))
      (dolist (param (rest params)) (write-char #\& out) (write-param param)))))

;;; anaphora!

(defmacro when1 (test &body body)
  (once-only (test) `(when ,test ,@body ,test)))

(defmacro awhen1 (test &body body)
  `(anaphora::anaphoric when1 ,test ,@body))

(defmacro and1 (test &rest rest)
  (once-only (test) `(and ,test ,@rest ,test)))

(defmacro aand1 (test &rest rest)
  `(anaphora::anaphoric and1 ,test ,@rest))

(defmacro with-aslots (slots form &body body)
  `(let ((it ,form)) (with-slots ,slots it ,@body)))

;;; json

(defun read-json (in)
  (let ((cl-json:*json-identifier-name-to-lisp* 'identity))
    (cl-json:decode-json-from-string in)))

(defun getjso (key &optional map)
  (if map (cdr (assoc key map :test #'string=))
      (lambda (map) (getjso key map))))

(defun mapjso (func map) (loop for (k . v) in map do (funcall func (string k) v)))

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

(defun jso-keys (jso) (mapcar-jso (lambda (k v) (declare (ignore v)) k) jso))

(defun mapjso* (thunk jso)
  (mapjso thunk jso)
  jso)

;;; random useful occasionally
(defun gapps-rate (from to)
  (getjso "rate" (read-json (drakma:http-request
                             "http://rate-exchange.appspot.com/currency"
                             :parameters `(("from" . ,from) ("to" . ,to))
                             :want-stream t))))
