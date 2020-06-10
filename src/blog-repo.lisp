(defpackage :cl-swbymabeweb.blog-repo
  (:use :cl :local-time)
  (:nicknames :blog-repo)
  (:export #:blog-entry
           #:make-blog-entry
           #:blog-entry-name
           #:blog-entry-date
           #:blog-entry-text
           ;; facade for repo access
           #:repo-get-latest
           #:repo-get-all
           #:repo-get-blog-entry
           ;; blog-repo class
           #:blog-repo-base
           ;; factory
           #:blog-repo-fac
           #:blog-repo-fac-init
           #:blog-repo-fac-get
           #:blog-repo-fac-clean)
  (:import-from #:serapeum
                #:->))

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

(defun make-blog-entry (name date text)
  (make-instance 'blog-entry :name name :date date :text text))

(defun blog-entry-p (entry)
  (typep entry 'blog-entry))

;; ---------------------------------------
;; blog repo -----------------------------
;; ---------------------------------------

(defclass blog-repo-base ()
  ()
  (:documentation "The base class for the repo."))

(defgeneric get-latest (blog-repo-base))
(defgeneric get-all (blog-repo-base))
(defgeneric get-for-name (blog-repo-base blog-name))

(defun repo-get-latest ()
  "Retrieves the latest entry of the blog."
  (get-latest (blog-repo-fac-get)))

(defun repo-get-all ()
  "Retrieves all available blog posts."
  (get-all (blog-repo-fac-get)))

(defun repo-get-blog-entry (name)
  "Retrieves a blog entry for the given name."
  (get-for-name (blog-repo-fac-get) name))

;; ---------------------------------------
;; blog repo factory ---------------------
;; ---------------------------------------
(defclass blog-repo-fac ()
  ((instance
    :initform nil
    :accessor instance
    :allocation :class))
  (:documentation "The blog repo factory."))

(defun blog-repo-fac-init (repo-instance)
  (setf (instance *blog-repo-fac*) repo-instance))
(defun blog-repo-fac-get ()
  (if (null (instance *blog-repo-fac*))
      (error "Set an instance first!")
      (instance *blog-repo-fac*)))
(defun blog-repo-fac-clean ()
  (setf (instance *blog-repo-fac*) nil))

(defvar *blog-repo-fac* (make-instance 'blog-repo-fac))
