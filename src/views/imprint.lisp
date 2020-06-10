(in-package :cl-user)
(defpackage :cl-swbymabeweb.view.imprint
  (:use :cl :cl-who :cl-locale :view.common)
  (:nicknames :view.imprint)
  (:export #:render))

(in-package :cl-swbymabeweb.view.imprint)

(defparameter *page-title* "Manfred Bergmann | Software Development | Imprint")

(defmacro content ()
  `(htm
    (:div :id "content"
          (with-content-table
            (content-headline (i18n "imprint_headline"))
            (content-subline (i18n "imprint_subline"))
            (with-content-line
              (:p
               "Manfred Bergmann" (:br)
               "Burgbergweg 8" (:br)
               "90559 Burgthann" (:br)
               "Germany" (:br)
               (:br)
               "E-Mail: webmaster (at) software-by-mabe.com" (:br))
              (:br)
              ,(i18n "imprint_text"))))))

(defun render ()
  (log:debug "Rendering imprint view.")
  (with-page *page-title*
    (content)))
