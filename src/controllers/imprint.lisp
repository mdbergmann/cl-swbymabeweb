(defpackage :cl-swbymabeweb.controller.imprint
  (:use :cl :cl-swbymabeweb.view.imprint)
  (:nicknames :controller.imprint)
  (:export #:index))

(in-package :cl-swbymabeweb.controller.imprint)

(defun index ()
  (log:debug "Imprint controller.")
  (view.imprint:render))
