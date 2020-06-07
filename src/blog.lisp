(defpackage :cl-swbymabeweb.blog
  (:use :cl :local-time)
  (:nicknames :blog)
  (:export #:blog-entry
           #:make-blog-entry
           #:get-entry-name
           #:get-entry-date
           #:get-entry-text
           #:get-latest))

(in-package :cl-swbymabeweb.blog)

(defclass blog-entry ()
  ((name :initform ""
         :type string
         :initarg :name
         :reader get-entry-name)
   (date :initform nil
         :type local-time:timestamp
         :initarg :date
         :reader get-entry-date)
   (text :initform ""
         :type string
         :initarg :text
         :reader get-entry-text)))

(defun make-blog-entry (name date text)
  (make-instance 'blog-entry :name name :date date :text text))

(defun get-latest ()
  "Retrieves the latest entry of the blog."
  (make-blog-entry "Foo" (now) "Some blog test"))
