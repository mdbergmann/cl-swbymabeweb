(defpackage cl-swbymabeweb.web
  (:use :cl
        :caveman2
        :cl-swbymabeweb.config)
  (:export :*web*))
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

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app)))
