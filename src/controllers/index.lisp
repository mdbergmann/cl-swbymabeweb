(defpackage :cl-swbymabeweb.controller.index
  (:use :cl :cl-swbymabeweb.view.index)
  (:nicknames :controller.index)
  (:export #:index)
  )

(in-package :cl-swbymabeweb.controller.index)

(defun index ()
  (view.index:render))
