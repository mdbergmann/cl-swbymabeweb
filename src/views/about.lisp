(defpackage :cl-swbymabeweb.view.about
  (:use :cl :cl-who :cl-i18n :view.common)
  (:nicknames :view.about)
  (:export #:render))

(in-package :cl-swbymabeweb.view.about)

(defparameter *page-title* "Manfred Bergmann | Software Development | About")

(defmacro content (content-fun-call)
  `(htm
    (:div :id "content"
          (with-content-table
            (content-headline #!"about_headline")
            (content-subline #!"about_subline")
            (with-content-line
              (str ,content-fun-call))))))

(defun render (content-fun)
  (log:debug "Rendering about view.")
  (with-page *page-title*
    (content (funcall content-fun))))
