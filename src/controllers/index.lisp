(defpackage :cl-swbymabeweb.controller
  (:use :cl)
  (:nicknames :controller)
  (:export #:controller-result))

(in-package :cl-swbymabeweb.controller)

(deftype controller-result ()
  "The controller output type"
  '(cons symbol string))


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
  (cons :ok (view.index:render)))
