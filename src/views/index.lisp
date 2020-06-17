(defpackage :cl-swbymabeweb.view.index
  (:use :cl :cl-who :view.common)
  (:nicknames :view.index)
  (:export #:render))

(in-package :cl-swbymabeweb.view.index)

(defparameter *page-title* "Manfred Bergmann | Software Development | Index")

(defun render ()
  (log:debug "Rendering index view.")
  (with-page *page-title*))
