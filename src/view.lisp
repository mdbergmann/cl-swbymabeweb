(in-package :cl-user)
(defpackage cl-swbymabeweb.view
  (:use :cl :spinneret)
  ;; (:import-from :cl-swbymabeweb.config
  ;;               :*template-directory*)
  (:import-from :caveman2
                :*response*
                :response-headers)
  (:export :render))
(in-package :cl-swbymabeweb.view)

(defparameter *template-registry* (make-hash-table :test 'equal))

(defun render (&optional env)
  (declare (ignore env))
  (with-html-string
    (:doctype)
    (:html
     (:head
      (:title "Foobar"))
     (:body
      (:b "Hello World")))))
