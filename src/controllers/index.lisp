(defpackage :cl-swbymabeweb.controller.index
  (:use :cl :controller :cl-swbymabeweb.view.index)
  (:nicknames :controller.index)
  (:export #:index)
  (:import-from #:serapeum
                #:->))

(in-package :cl-swbymabeweb.controller.index)

(-> index () controller-result)
(defun index ()
  "Not yet sure what this should do, where it delegates to."
  (log:debug "Index controller.")
  (controller.blog:index))
  ;;(cons :ok (view.index:render)))
