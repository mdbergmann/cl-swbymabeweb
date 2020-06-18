(defpackage :cl-swbymabeweb.controller.index
  (:use :cl :controller :cl-swbymabeweb.view.index)
  (:nicknames :controller.index)
  (:export #:index)
  (:import-from #:serapeum
                #:->))

(in-package :cl-swbymabeweb.controller.index)

(-> index () controller-result)
(defun index ()
  (log:debug "Index controller.")
  (cons :ok (view.index:render)))
