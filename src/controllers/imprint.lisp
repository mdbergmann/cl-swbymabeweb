(defpackage :cl-swbymabeweb.controller.imprint
  (:use :cl :controller :cl-swbymabeweb.view.imprint)
  (:nicknames :controller.imprint)
  (:export #:index)
  (:import-from #:serapeum
                #:->))

(in-package :cl-swbymabeweb.controller.imprint)

(-> index () controller-result)
(defun index ()
  "Just calls `about' view's `render' function."
  (log:debug "Imprint controller.")
  (cons :ok (view.imprint:render)))
