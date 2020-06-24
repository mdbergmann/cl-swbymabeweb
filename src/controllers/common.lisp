(defpackage :cl-swbymabeweb.controller
  (:use :cl)
  (:nicknames :controller)
  (:local-nicknames (:md :3bmd))
  (:import-from #:cl-swbymabeweb.config
                #:*content-directory*)
  (:export #:controller-result
           #:make-controller-result
           #:load-content-resource))

(in-package :cl-swbymabeweb.controller)

(deftype controller-result ()
  "The controller output type"
  '(cons symbol string))

(defun make-controller-result (first second)
  "Convenience function to create a new controller result.
But also used for visibility of where the result is created."
  (cons first second))

(defun load-content-resource (resource)
  (let ((stream (make-string-output-stream))
        (resource-file (merge-pathnames resource *content-directory*)))
    (md:parse-and-print-to-stream resource-file stream)
    (get-output-stream-string stream)))

