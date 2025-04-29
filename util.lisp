(defpackage #:scalpl.util
  (:use #:c2cl #:anaphora #:parse-float #:decimals
        #:string-case #:local-time #:split-sequence)
  (:export ; if these are complex enough that their documentation
          ;; string overflows into the format restandardizatation
   ;; debate, then they may belong in someone else's utility lib.
   #:projugate                          ; how useful is it, really?
   #:icbrt
   #:shorten-uid
   #:once-only
   #:shallow-copy
   #:dbz-guard
   #:slot-reduce #:slot-reducer #:slot-setter #:aslot-setter

:PRICE-PRECISION-PROBLEM ;yes
   
   #:aand1 #:awhen1 #:amvp1
   #:parse-price
   #:parse-float
   #:string-octets #:octets
   #:rehome-symbol
   #:rehome-class
   #:string-case
   #:concatenate-url-parameters
   #:urlencode-params
   #:url-decode
   #:break-errors
   #:kw #:mvwrap #:mapsym
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
   ;; #:mapjso*
   #+clozure #:memory-usage
   #:split-sequence
   #:mapreduce				; IT IS NOT FROM ALEXANDRIA
   ))

(in-package #:scalpl.util)

;;; rescued from mpex.lisp
(defun parse-rfc1123-timestring (timestring &key) ; TODO: fail-on-error etc
  (flet ((short-month-index (short-name)
           (string-case (short-name)
             ("Jan" 1) ("Feb" 2) ("Mar" 3) ("Apr" 4) ("May" 5) ("Jun" 6)
             ("Jul" 7) ("Aug" 8) ("Sep" 9) ("Oct" 10) ("Nov" 11) ("Dec" 12))))
    (destructuring-bind (dd mmm yyyy hhmmss zone)
        (cdr (split-sequence #\Space timestring))
      (parse-rfc3339-timestring
       (format nil "~A-~2,'0D-~AT~A~A"
               yyyy (short-month-index mmm) dd hhmmss zone)))))

(defconstant +time-offset-units+
  '(:nsec :sec :minute :hour :day :day-of-week :month :year)
  "the list of valid `UNIT' arguments for `LOCAL-TIME:TIMESTAMP+', etc.")

;;; this could almost be part of local-time ,.!
(deftype time-amount (&optional unit (amount 'unsigned-byte))
  "the range of valid &rest tails for `LOCAL-TIME:TIMESTAMP+'"
  ;; (unless (eq unit '*)  ;; CHAT IS THIS CRETE???
  ;;   (check-type unit (member +time-offset-units+)))
  `(cons ,amount (cons ,(if (not (eq unit '*)) `(eql ,unit)
                            `(member .,+time-offset-units+)))))

;;; naming problems is the only liability
;;; 'projugate' evokes 'congress opposes progress'
;;; 'complement' evokes the angular interpretation
;;; #:|CO: see, oh, I don't know: it beats about the
;;;  bush to show a link to wikt/complementarity#English|#
;;; 'supplement' is perhaps the correct appelation
(defun projugate (complex) (- (conjugate complex)))

(defun icbrt (x &aux (l (integer-length x))) ; [War2003] pp.211-212
  (do ((s (* 3 l) (- s 3)) (y 0 (ash y 1)) (y2 0 (ash y2 2)))
      ((minusp s) (ash y -1))
    (let ((b (ash (1+ (* 3 (+ y y2))) s)))
      (when (>= x b) (decf x b) (incf y2 (1+ (* 2 y))) (incf y)))))

;;; hyphens within alphanumeric identifiers frequently denote
;;; semantic divisions, in a manner similar to how null bytes
;;; and colons within addresses designators notated according
;;; to the sixth version of the internet protocol, frequently
;;; denote physical discontinuity.
(defun shorten-uid (uid &optional (separator #\-) (field 0))
  (check-type uid string "Identifiers must be strings")
  ;; (ctypecase uid (string) (null (setf uid "")))
  ;; (check-type separator character "Strings must contain characters")
  (check-type field (signed-byte 8) "I've got a one-track mind that leads nowhere")
  (cond
    ((null separator) (subseq uid (isqrt (length uid))))
    ((zerop field) (subseq uid 0 (position separator uid)))))
;;; consider reading various UUID documents, before writing on

;;; WHY'LL YOU WRITE THIS POETRY, PETRONVK THE ELDER GOES DEAF
;; (defun chr (real)
;;   " ;_; was I a good function? // No. ~"
;;   (funcall (if (plusp real) #'identity #'char-upcase)
;;            (char "hwfumonerylsicazjx"   ; VTQPKGDB ???
;;                  (floor (* (abs real) #xFF) #xF))))

(defun strftime (&optional datep &aux bits)
  (let ((data (multiple-value-list  ; my kingdom for a stack!
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

;;; used (exp once) ; not enough for defmacro
(defun subseq* (sequence start &optional end)
  (handler-case (subseq sequence start end)
    (error () (subseq sequence start))))

(defun kw (thing) (intern (string-upcase (string thing)) :keyword))

(defmacro mvwrap (slot function)
  `(apply 'values (and ,slot `(,,(kw slot) ,(,function ,slot)))))

(defun mapsym (function result-type names &rest args)
  (apply #'map result-type function (mapcar 'symbol-value names) args))

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

;;; originally, this was only for exploratory concision!
;;; as was suggested in #lisp, each occurrence in commit
;;; codes is a red flag that some abstraction is missing
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
    (get-setf-expansion `(slot-value (slot-reduce ,root ,@path) ',end)
                        env)))
;;; NOW BUY SOME OPIUM PLEASE AND STOP BOTHERING ME WITHOUT PAYMENT

(defun rehome-symbol (symbol target &aux (source (symbol-package symbol)))
  (unintern symbol source) (import symbol target) (import symbol source))

(defgeneric rehome-class (class target)
  (:method ((class symbol) target)
    (rehome-class (find-class class) (find-package target)))
  (:method ((class class) (target package))
    (mapcar (lambda (symbol) (rehome-symbol symbol target))
            (cons (class-name class)
                  (mapcar 'slot-definition-name
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
  (let ((dot (or (position #\. price-string) (length price-string))))
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

(defun concatenate-url-parameters (alist)
  (with-output-to-string (out) ;_; this code smells like an office building
    (flet ((write-parameter (cons) (format out "~A=~A" (car cons) (cdr cons))))
      (awhen (first alist) (write-parameter it))
      (dolist (cons (rest alist)) (write-char #\& out) (write-parameter cons)))))

(defun urlencode-params (alist &optional (format :utf8))
  (flet ((urlencode (thing)             ;_; and this function
           (drakma:url-encode (princ-to-string thing) format)))
    (concatenate-url-parameters         ;   looks like a life
     (mapcar (lambda (cons)             ;   filled with regrets
               (cons (urlencode (car cons)) (urlencode (cdr cons))))
             alist))))                  ;   and hateful squalor

(defun url-decode (string)
  "Returns a URL-decoded version of the string STRING using the
external format EXTERNAL-FORMAT."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in () ()) while char
            do (write-char (cond ((char= char #\+) #\Space)
                                 ((char= char #\%)
                                  (code-char (+ (* 16 (digit-char-p
                                                       (read-char in) 16))
                                                (digit-char-p (read-char in) 16))))
                                 (t char))
                           out)))))

;;; wipe-flush effect-afford

(defun mapreduce (map reduce &rest args) ; alexandria signals dead brains
  (reduce reduce (apply 'mapcar map args))) ; will-to-live false said now

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
  (let ((json:*real-handler*
         (lambda (string)
           (or (ignore-errors (parse-decimal-number string))
               ;; (warn "CL:IGNORE-ERRORS CONSIDERED HARMFUL")
               (parse-float string :type 'rational))))
        ;; (json:*json-array-type* 'vector) ; breaks lots of things!
        ;; this prevents memory leaks from random JSON keys
        (json:*json-identifier-name-to-lisp* 'identity))
    (ctypecase in
      (string (json:decode-json-from-string in))
      (stream (json:decode-json in)))))

(defun decode-json (arg)
  (read-json (flexi-streams:octets-to-string arg :external-format :utf8)))

;;; WARNING: Careless use of this function could cause memory leaks,
;;; similarly to issue that was encountered with Drakma, Chunga, and Kraken
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
