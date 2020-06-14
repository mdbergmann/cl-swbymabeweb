(in-package :cl-user)
(defpackage :cl-swbymabeweb.view.imprint
  (:use :cl :cl-who :cl-locale :view.common)
  (:nicknames :view.imprint)
  (:export #:render))

(in-package :cl-swbymabeweb.view.imprint)

(defparameter *page-title* "Manfred Bergmann | Software Development | Imprint")

(defmacro content (content-fun-call)
  `(htm
    (:div :id "content"
          (with-content-table
            (content-headline (i18n "imprint_headline"))
            (content-subline (i18n "imprint_subline"))
            (with-content-line
              (str ,content-fun-call))))))

(defun render (content-fun)
  (log:debug "Rendering imprint view.")
  (with-page *page-title*
    (content (funcall content-fun))))
