(defpackage :cl-swbymabeweb.controller.about
  (:use :cl :controller :cl-swbymabeweb.view.about)
  (:nicknames :controller.about)
  (:export #:index)
  (:import-from #:serapeum
                #:->))

(in-package :cl-swbymabeweb.controller.about)

(-> index () controller-result)
(defun index ()
  "Just calls `about' view's `render' function."
  (log:debug "About controller.")
  (cons :ok (view.about:render)))
