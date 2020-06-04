(defpackage :cl-swbymabeweb.view.common
  (:use :cl :spinneret)
  (:nicknames :view.common)
  (:export #:with-page))

(in-package :cl-swbymabeweb.view.common)

(defmacro with-page ((&key title) &rest body)
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title))
      (:body ,@body))))
