(defpackage :cl-swbymabeweb.blog-repo
  (:use :cl :local-time)
  (:nicknames :blog-repo)
  (:export #:blog-entry
           #:make-blog-entry
           #:blog-entry-name
           #:blog-entry-date
           #:blog-entry-text
           ;; convenience method for repo access
           #:repo-get-latest
           #:repo-get-blog-entry
           ;; blog-repo class
           #:blog-repo-base
           #:get-latest
           #:get-for-name
           ;; factory
           #:blog-repo-fac
           #:repo-init-with
           #:repo-get-instance
           #:repo-clean))

(in-package :cl-swbymabeweb.blog-repo)

;; ---------------------------------------
;; blog repo model -----------------------
;; ---------------------------------------
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

;; ---------------------------------------
;; blog repo -----------------------------
;; ---------------------------------------
(defclass blog-repo-base ()
  ()
  (:documentation "The base class for the repo."))

(defgeneric get-latest (blog-repo-base))
(defgeneric get-for-name (blog-repo-base blog-name))

(defun make-blog-entry (name date text)
  (make-instance 'blog-entry :name name :date date :text text))

(defun repo-get-latest ()
  "Retrieves the latest entry of the blog."
  (get-latest (repo-get-instance)))

(defun repo-get-blog-entry (name)
  "Retrieves a blog entry for the given name."
  (get-for-name (repo-get-instance) name))

;; ---------------------------------------
;; blog repo factory ---------------------
;; ---------------------------------------
(defvar *blog-repo-fac* (make-instance 'blog-repo-fac))
(defclass blog-repo-fac ()
  ((instance
    :initform nil
    :accessor instance
    :allocation :class))
  (:documentation "The blog repo factory."))

(defun repo-init-with (repo-instance)
  (setf (instance *blog-repo-fac*) repo-instance))
(defun repo-get-instance ()
  (if (null (instance *blog-repo-fac*))
      (error "Set an instance first!")
      (instance *blog-repo-fac*)))
(defun repo-clean ()
  (setf (instance *blog-repo-fac*) nil))
