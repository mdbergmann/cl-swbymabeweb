(defpackage :cl-swbymabeweb.controller
  (:use :cl)
  (:nicknames :controller)
  (:local-nicknames (:md :3bmd))
  (:import-from #:cl-swbymabeweb.config
                #:*content-directory*)
  (:export #:controller-result
           #:load-content-resource))

(in-package :cl-swbymabeweb.controller)

(deftype controller-result ()
  "The controller output type"
  '(cons symbol string))

(defun load-content-resource (resource)
  (let ((stream (make-string-output-stream))
        (resource-file (merge-pathnames resource *content-directory*)))
    (md:parse-and-print-to-stream resource-file stream)
    (get-output-stream-string stream)))

