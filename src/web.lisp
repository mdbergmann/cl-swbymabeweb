(defpackage cl-swbymabeweb.web
  (:use :cl
        :caveman2
        :cl-swbymabeweb.config)
  (:export :*web*)
  (:import-from #:lack.response
                #:response-status))
(in-package :cl-swbymabeweb.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  (log:debug "Index route called.")
  (controller.index:index))

(defroute "/imprint" ()
  (log:debug "Imprint route called.")
  (controller.imprint:index))

(defroute "/about" ()
  (log:debug "About route called.")
  (controller.about:index))

(defroute "/blog" ()
  (log:debug "Blog route called.")
  (let ((result (controller.blog:index)))
    (case (car result)
      (:ok (cdr result))
      (t (setf (response-status *response*) 400)))))

(defroute "/blog/:name" (&key name)
  (log:debug "Blog route called with name: " name)
  (let ((result (controller.blog:for-blog-name name)))
    (case (car result)
      (:ok (cdr result))
      (:not-found-error (setf (response-status *response*) 404))
      (t (setf (response-status *response*) 400)))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app)))
