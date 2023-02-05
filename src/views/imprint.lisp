(in-package :cl-user)
(defpackage :cl-swbymabeweb.view.imprint
  (:use :cl :cl-who :cl-i18n :view.common)
  (:nicknames :view.imprint)
  (:export #:render))

(in-package :cl-swbymabeweb.view.imprint)

(defparameter *page-title* "Manfred Bergmann | Software Development | Imprint")

(defun render (content-fun)
  (log:debug "Rendering imprint view.")
  (with-page (*page-title*
              "
.sub_content {
    width: 100%;
}
")
             (:div :class "sub_content"
                   (:h3 (str #!"imprint_headline"))
                   (:h4 (str #!"imprint_subline"))
                   (:div (str (funcall content-fun))))))
