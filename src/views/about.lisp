(defpackage :cl-swbymabeweb.view.about
  (:use :cl :cl-who :cl-locale :view.common)
  (:nicknames :view.about)
  (:export #:render))

(in-package :cl-swbymabeweb.view.about)

(defparameter *page-title* "Manfred Bergmann | Software Development | About")

(defmacro content ()
  `(htm
   (:div :id "content"
         (with-content-table
           (content-headline (i18n "about_headline"))
           (content-subline (i18n "about_subline"))
           (with-content-line
             ,(i18n "mabe_mission_text"))
           (content-subline (i18n "background_subline"))
           (with-content-line
             ,(i18n "background_text"))))))

(defun render ()
  (log:debug "Rendering about view.")
  (with-page *page-title*
    (content)))
