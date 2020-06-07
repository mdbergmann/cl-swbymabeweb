(defpackage :cl-swbymabeweb.controller.about
  (:use :cl :cl-swbymabeweb.view.about)
  (:nicknames :controller.about)
  (:export #:index))

(in-package :cl-swbymabeweb.controller.about)

(defun index ()
  (log:debug "About controller.")
  (view.about:render))
