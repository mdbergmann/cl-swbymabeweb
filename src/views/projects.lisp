1(defpackage :cl-swbymabeweb.view.projects
  (:use :cl :cl-who :cl-i18n :view.common)
  (:nicknames :view.projects)
  (:export #:render))

(in-package :cl-swbymabeweb.view.projects)

(defparameter *page-title* "Manfred Bergmann | Software Development | Projects")

(defmacro content (content-fun-call)
  `(htm
    (:div :id "content"
          (with-content-table
            (content-headline #!"projects_headline")
            (content-subline #!"projects_subline")
            (with-content-line
              (str ,content-fun-call))))))

(defun render (content-fun)
  (log:debug "Rendering projects view.")
  (with-page (*page-title*
              "
.sub_content {
    width: 100%;
}
")
             (:div :class "sub_content"
                   (:h3 (str #!"projects_headline"))
                   (:h4 (str #!"projects_subline"))
                   (:div (str (funcall content-fun))))))
