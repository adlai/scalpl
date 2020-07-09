(defpackage #:scalpl.util
  (:use #:c2cl #:anaphora #:parse-float
        #:string-case #:local-time #:split-sequence)
  (:export #:once-only
           #:shallow-copy
           #:dbz-guard
           #:slot-reduce #:slot-reducer #:slot-setter #:aslot-setter
           #:aand1 #:awhen1 #:amvp1
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
           #:decode-json
           #:getjso
           #:mapjso
           #:mapcar-jso
           #:jso-keys
           #:with-json-slots
           #:mapjso*
           #:short-month-index
           #:parse-rfc1123-timestring
           #+clozure #:memory-usage
           ))

(in-package #:scalpl.util)

;;; Actually useful

(defun short-month-index (short-name)
  (string-case (short-name)
    ("Jan" 1) ("Feb" 2) ("Mar" 3) ("Apr" 4) ("May" 5) ("Jun" 6)
    ("Jul" 7) ("Aug" 8) ("Sep" 9) ("Oct" 10) ("Nov" 11) ("Dec" 12)))

;;; TODO: row, row, row, your boat, gently up the stream...
(defun parse-rfc1123-timestring (timestring &key) ; TODO: fail-on-error etc
  (destructuring-bind (dd mmm yyyy hhmmss zone)
      (cdr (split-sequence #\Space timestring))
    (let ((temp (format nil "~A-~2,'0D-~AT~A~A"
                        yyyy (short-month-index mmm) dd hhmmss zone)))
      (declare (dynamic-extent temp))
      (parse-rfc3339-timestring temp))))

(defun strftime (&optional datep &aux bits)
  (let ((data (multiple-value-list      ; my kingdom for a stack!
               (decode-universal-time (get-universal-time)))))
    (symbol-macrolet ((next (princ-to-string (pop data))))
      (macrolet ((collect (&rest xs)
                   `(progn ,@(mapcar (lambda (x) `(push ,x bits)) xs))))
        (collect next ":" next ":" next)
        (when datep (collect " " next "-" next)))))
  (apply 'concatenate 'string bits))

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

(defmacro aslot-setter (new-value &rest slots)
  `(lambda (it) (setf (slot-reduce it ,@slots) ,new-value)))

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

(defun urlencode-params (params &optional (format :utf8))
  (with-output-to-string (out)
    (labels ((urlencode (thing)
               (drakma:url-encode (princ-to-string thing) format))
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

(defmacro amvp1 (values-form &body body)
  `(apply #'values (aprog1 (multiple-value-list ,values-form) ,@body)))

(defmacro with-aslots (slots form &body body)
  `(let ((it ,form)) (with-slots ,slots it ,@body)))

;;; json

(defun read-json (in)
  (let ((cl-json:*real-handler* (lambda (x) (parse-float x :type 'rational)))
        (cl-json:*json-identifier-name-to-lisp* 'identity))
    (ctypecase in
      (string (cl-json:decode-json-from-string in))
      (stream (cl-json:decode-json in)))))

(defun decode-json (arg)
  (read-json (flexi-streams:octets-to-string arg :external-format :utf8)))

(defun getjso (key &optional (map () map-p))
  (if (not map-p) (lambda (map) (getjso key map))
      (cdr (assoc (intern (string key) :keyword) map :test #'string=))))

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

;;; memory introspection, where supported
#+clozure
(defun memory-usage ()
  "Scrapes a memory usage estimate from `ROOM' output"
  (let ((parts (split-sequence
                #\( (with-output-to-string (*standard-output*) (room)))))
    (reduce
     '+ (mapcar (lambda (part) (parse-integer part :junk-allowed t))
                (list (nth 1 parts) (nth 4 parts) (nth 7 parts))))))
