(defpackage :cl-swbymabeweb.view.about
  (:use :cl :cl-who :cl-i18n :view.common)
  (:nicknames :view.about)
  (:export #:render))

(in-package :cl-swbymabeweb.view.about)

(defparameter *page-title* "Manfred Bergmann | Software Development | About")

(defun render (content-fun)
  (log:debug "Rendering about view.")
  (with-page (*page-title*
              "
.sub_content {
    width: 100%;
}
")
             (:div :class "sub_content"
                   (:h3 (str #!"about_headline"))
                   (:h4 (str #!"about_subline"))
                   (:div (str (funcall content-fun))))))
