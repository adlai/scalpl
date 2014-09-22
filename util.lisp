(defpackage #:scalpl.util
  (:use #:cl #:st-json)
  (:export #:with-json-slots
           #:mapcar-jso
           #:mapjso*
           #:urlencode-params
           #:awhen1 #:aand1
           #:string-octets
           #:parse-price
           ))

(in-package #:scalpl.util)

;;; Actually useful

;;; TODO: define these conditions!
(defun parse-price (price-string decimals)
  (let ((dot (position #\. price-string)))
    (multiple-value-bind (int end) (parse-integer (remove #\. price-string))
      (let ((delta (- end dot decimals)))
        (case (signum delta)
          (-1 (warn "Price string ~S specifies only ~D decimal~:*~P (short by ~D)"
                    price-string (- end dot) (- delta))
              (* int (expt 10 (- delta))))
          (0 int)
          (+1 (warn "Price string ~S specifies ~D decimal~:*~P (~D too many)"
                    price-string (- end dot) delta)
              (floor int (expt 10 delta))))))))

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
