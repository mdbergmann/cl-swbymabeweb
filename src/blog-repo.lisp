(defpackage :cl-swbymabeweb.blog-repo
  (:use :cl :local-time)
  (:nicknames :blog-repo)
  (:export #:blog-entry
           #:make-blog-entry
           #:blog-entry-name
           #:blog-entry-date
           #:blog-entry-text
           #:repo-get-latest
           #:repo-get-blog-entry))

(in-package :cl-swbymabeweb.blog-repo)

(defclass blog-entry ()
  ((name :initform ""
         :type string
         :initarg :name
         :reader blog-entry-name)
   (date :initform nil
         :type local-time:timestamp
         :initarg :date
         :reader blog-entry-date)
   (text :initform ""
         :type string
         :initarg :text
         :reader blog-entry-text)))

(defun make-blog-entry (name date text)
  (make-instance 'blog-entry :name name :date date :text text))

(defun repo-get-latest ()
  "Retrieves the latest entry of the blog."
  (cons :ok nil))

(defun repo-get-blog-entry (name) (cons :ok nil))
