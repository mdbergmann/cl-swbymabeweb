(defpackage :cl-swbymabeweb.blog-repo
  (:use :cl)
  (:nicknames :blog-repo)
  (:export #:blog-entry
           #:blog-entry-p
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
           #:blog-repo-default
           ;; factory
           #:blog-repo-fac
           #:blog-repo-fac-init
           #:blog-repo-fac-get
           #:blog-repo-fac-clean)
  (:import-from #:serapeum
                #:->
                #:~>
                #:~>>)
  (:import-from #:cl-date-time-parser
                #:parse-date-time))

(in-package :cl-swbymabeweb.blog-repo)

;; ---------------------------------------
;; blog repo model -----------------------
;; ---------------------------------------

(defclass blog-entry ()
  ((name :initform ""
         :type string
         :initarg :name
         :reader blog-entry-name
         :documentation "the blog name, the filename minus the date.")
   (date :initform nil
         :type fixnum
         :initarg :date
         :reader blog-entry-date
         :documentation "universal timestamp")
   (text :initform ""
         :type string
         :initarg :text
         :reader blog-entry-text
         :documentation "The content of the file")))

(defun make-blog-entry (name date text)
  (make-instance 'blog-entry :name name :date date :text text))

(defun blog-entry-p (entry)
  (typep entry 'blog-entry))

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

;; blog repo default impl -----------------------

(defclass blog-repo-default (blog-repo-base)
  ((blog-folder :initarg :blog-folder
                :initform nil
                :documentation "The folder where the blog posts are expected.")))

(defmethod get-all ((self blog-repo-default))
  (with-slots (blog-folder) self
    (log:debug "Get all in: " blog-folder)
    (mapcar #'file-to-blog-entry
            (remove-if-not #'allowed-file-ext-p
                           (uiop:directory-files blog-folder)))))

(defun allowed-file-ext-p (file)
  (or (str:ends-with-p ".html" (namestring file))
      (str:ends-with-p ".md" (namestring file))))

(defun file-to-blog-entry (file)
  "Reads file and takes data from it to create a `blog-entry'"
  (~> file
      (file-namestring)
      (blog-entry-name-and-datestring)
      ((lambda (name-and-datestring)
         (destructuring-bind (blog-name datestring) name-and-datestring
           (log:debug "Have blog-name: ~a and datestring: ~a~%" blog-name datestring)
           (make-blog-entry blog-name
                            (datestring-to-universal-time datestring)
                            (read-file-content-as-string file)))))))
      
(defun blog-entry-name-and-datestring (filename)
  "takes the filename and replaces any '_' with space, plus reomves the file extension."
  (~>> filename
       (str:replace-all "_" " ")
       (str:substring 0
                      (- (length filename)
                         (1+ (length (pathname-type filename)))))
       (str:split "-")))

(defun datestring-to-universal-time (datestring)
  (parse-date-time datestring))

(defun read-file-content-as-string (file)
  (uiop:read-file-string file))
