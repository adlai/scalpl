;;;; db.lisp

(defpackage #:scalpl.db
  (:use #:cl #:dbi #:anaphora #:local-time #:scalpl.util #:scalpl.exchange))

(in-package #:scalpl.db)

(defun sql (db sql &rest args &aux rows)
  (let ((query (apply #'format nil sql args)))
    (format t "~&~A" query)
    (let ((ret (dbi:execute (dbi:prepare db query))))
      (loop (aif (dbi:fetch ret) (push it rows)
                 (return (reverse rows)))))))

(defun format-table (db table columns)
  (tagbody :retry
   (handler-case
       (sql db "CREATE TABLE ~A ( ~{~{~S ~A~}~#[~:;, ~]~} );" table columns)
     (dbi.error:<dbi-programming-error> (error)
       (if (search "already exists" (slot-value error 'dbi.error::message))
           (progn (cerror "Clobber existing table" "Table already exists")
                  (sql db "DROP TABLE ~A" table) (go :retry))
           (error error))))))
