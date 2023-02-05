(defpackage :cl-swbymabeweb.view.index
  (:use :cl :cl-who :cl-i18n :view.common)
  (:nicknames :view.index)
  (:export #:render))

(in-package :cl-swbymabeweb.view.index)

(defparameter *page-title* "Manfred Bergmann | Software Development | Index")

(defun render ()
  (log:debug "Rendering index view.")
  (with-page (*page-title*
              "
.sub_content {
    width: 100%;
}
")
             (:div :class "sub_content"
                   (:div :style "text-align: center;"
                         (:p :style "font-size: 36pt;" (str #!"index_hello!"))
                         (:p (str #!"index_here_is_my_blog...") (:a :href "/blog" "[blog]"))))))
