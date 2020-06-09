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
  (cdr (controller.index:index)))

(defroute "/imprint" ()
  (log:debug "Imprint route called.")
  (cdr (controller.imprint:index)))

(defroute "/about" ()
  (log:debug "About route called.")
  (cdr (controller.about:index)))

(defroute "/blog" ()
  (log:debug "Blog route called.")
  (let ((result (controller.blog:index)))
    (case (car result)
      (:ok (cdr result))
      (t (progn
           (setf (response-status *response*) 400)
           "Undefined error!")))))

(defroute "/blog/:name" (&key name)
  (log:debug "Blog route called with name: " name)
  (let ((result (controller.blog:for-blog-name name)))
    (case (car result)
      (:ok (cdr result))
      (:not-found-error (progn
                          (setf (response-status *response*) 404)
                          (cdr result)))
      (t (progn
           (setf (response-status *response*) 400)
           "Undefined error!")))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app)))
