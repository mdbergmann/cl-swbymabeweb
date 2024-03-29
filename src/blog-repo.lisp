(defpackage :cl-swbymabeweb.blog-repo
  (:use :cl :sento.agent)
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
           #:repo-get-for-name
           ;; blog-repo class
           #:*blog-repo*
           #:blog-repo-base
           #:blog-repo-default)
  (:local-nicknames (:dtp :cl-date-time-parser))
  (:local-nicknames (:md :3bmd))
  (:import-from #:serapeum
                #:~>
                #:~>>))

(in-package :cl-swbymabeweb.blog-repo)

(defparameter *blog-repo* nil
  "the blog repo instance based on `blog-repo-base`.
It is possible to override this parameter for tests with a mock implementation.")

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
         :initarg :date
         :reader blog-entry-date
         :documentation "universal timestamp")
   (text :initform ""
         :type string
         :initarg :text
         :reader blog-entry-text
         :documentation "The content of the file")))

(defmethod print-object ((obj blog-entry) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (name date text) obj
      (format stream "name: ~a, date: ~a, size: ~a~%" name date (length text)))))

(defun make-blog-entry (name date text)
  (make-instance 'blog-entry :name name :date date :text text))

(defun blog-entry-p (entry)
  (typep entry 'blog-entry))

;; ---------------------------------------
;; blog repo facade ----------------------
;; ---------------------------------------

(defun repo-get-latest ()
  "Retrieves the latest entry of the blog."
  (cons :ok (get-latest *blog-repo*)))

(defun repo-get-all ()
  "Retrieves all available blog posts."
  (cons :ok (get-all *blog-repo*)))

(defun repo-get-for-name (name)
  "Retrieves a blog entry for the given name."
  (let ((result (get-for-name *blog-repo* name)))
    (if result
        (cons :ok result)
        (progn
          (log:warn "Blog entry for name '~a' not found!" name)
          (cons :not-found-error
                (format nil "Blog post with name: ~a does not exist!" name))))))

;; ---------------------------------------
;; blog repo class -----------------------
;; ---------------------------------------

(defclass blog-repo-base ()
  ()
  (:documentation "The base class for the repo."))

(defgeneric get-latest (blog-repo-base))
(defgeneric get-all (blog-repo-base))
(defgeneric get-for-name (blog-repo-base blog-name))

;; blog cache agent

(defun blog-agent-get-all (blog-agent)
  (agent-get blog-agent (lambda (blogs) blogs)))

(defun blog-agent-init (blog-agent blog-folder)
  (agent-update blog-agent (lambda (old-state)
                             (declare (ignore old-state))
                             (handler-case
                                 (load-blog-entries blog-folder)
                               (error (c)
                                 (let ((errMsg (format nil "Error on loading blog entries: ~a" c)))
                                   (log:error errMsg)
                                   nil))))))

(defun blog-agent-reinit ()
  (with-slots (blog-agent blog-folder) *blog-repo*
    (blog-agent-init blog-agent blog-folder)))

;; blog repo default impl -----------------------

(defclass blog-repo-default (blog-repo-base)
  ((blog-folder :initarg :blog-folder
                :initform (error "The blog-folder cannot be empty!")
                :documentation "The folder where the blog files are located")
   (blog-agent :initform (make-agent (lambda () nil))
               :documentation "The caching blog agent")
   (decorated-repo :initform nil
                   :documentation "The decorated repo"))
  (:documentation "A blog-repo implementation that caches entries using an agent."))

(defmethod initialize-instance :after ((self blog-repo-default) &key)
  (with-slots (decorated-repo blog-folder blog-agent) self
    (setf decorated-repo (make-instance 'blog-repo-direct :blog-folder blog-folder))
    (blog-agent-init blog-agent blog-folder)))

(defmethod get-all ((self blog-repo-default))
  (with-slots (blog-agent) self
    (blog-agent-get-all blog-agent)))

(defmethod get-latest ((self blog-repo-default))
  (with-slots (blog-agent) self
    (first (blog-agent-get-all blog-agent))))

(defmethod get-for-name ((self blog-repo-default) name)
  (log:debug "Finding blog: '~a'~%" name)
  (with-slots (blog-agent) self
    (find-blog-for-name name (blog-agent-get-all blog-agent))))

;; blog repo direct implementation

(defclass blog-repo-direct (blog-repo-base)
  ((blog-folder :initarg :blog-folder
                :initform (error "The blog-folder cannot be empty!")
                :documentation "The folder where the blog files are located"))
  (:documentation "A blog-repo that loads blog entries from file for each call."))

(defmethod get-all ((self blog-repo-direct))
  (with-slots (blog-folder) self
    (load-blog-entries blog-folder)))

(defmethod get-latest ((self blog-repo-direct))
  (with-slots (blog-folder) self
    (first (load-blog-entries blog-folder))))

(defmethod get-for-name ((self blog-repo-direct) name)
  (log:debug "Finding blog: '~a'~%" name)
  (with-slots (blog-folder) self
    (find-blog-for-name name (load-blog-entries blog-folder))))

;; ----------- utility functions -------------

(defun find-blog-for-name (name blog-entries)
  (find name blog-entries :key #'blog-entry-name :test #'string-equal))

(defun load-blog-entries (blog-folder)
  (log:debug "Get all in: " blog-folder)
  (~> blog-folder
      (filter-files)
      (to-blog-entries)
      (sort-for-date)))

(defun filter-files (folder)
  (~> folder
      (uiop:directory-files)
      (remove-if-not #'allowed-file-p _)))

(defun to-blog-entries (files)
  (mapcar #'file-to-blog-entry files))

(defun sort-for-date (blog-entries)
  (sort blog-entries #'> :key #'blog-entry-date))

(defun allowed-file-p (file)
  (let ((filename (file-namestring file)))
    (and (or (str:ends-with-p ".html" filename)
             (str:ends-with-p ".md" filename))
         (str:containsp "---" filename)
         (str:digitp (str:substring 0 8 filename)))))

(defun file-to-blog-entry (file)
  "Reads file and takes data from it to create a `blog-entry'"
  (~> file
      (file-namestring)
      (blog-entry-datestring-and-name)
      ((lambda (datestring-and-name)
         (destructuring-bind (datestring name) datestring-and-name
           (log:debug "Have blog-name: '~a' and datestring: '~a'~%" name datestring)
           (make-blog-entry name
                            (datestring-to-universal-time datestring)
                            (read-file-content-as-string file)))))))
      
(defun blog-entry-datestring-and-name (filename)
  "Takes the filename and replaces any '_' with space, plus removes the file extension."
  (~>> filename
       (str:replace-all "_" " ")
       (str:substring 0
                      (- (length filename)
                         (1+ (length (pathname-type filename)))))
       (str:split "---")))

(defun datestring-to-universal-time (datestring)
  (dtp:parse-date-time datestring))

(defun read-file-content-as-string (file)
  (html-or-md file (pathname-type file)))

(defun html-or-md (file extension)
  (if (string= "html" extension)
      (uiop:read-file-string file)
      (convert-md-to-html file)))

(defun convert-md-to-html (file)
  (setf 3bmd-code-blocks:*code-blocks* t)
  (setf 3bmd-code-blocks:*renderer* :nohighlight)
  
  (let ((stream (make-string-output-stream)))
    (md:parse-and-print-to-stream file stream)
    (get-output-stream-string stream)))
