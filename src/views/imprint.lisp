(defpackage :cl-swbymabeweb.view.imprint
  (:use :cl :spinneret :cl-locale :view.common)
  (:nicknames :view.imprint)
  (:export #:render))

(in-package :cl-swbymabeweb.view.imprint)

(defparameter *page-title* "Manfred Bergmann | Software Development | Imprint")

(defmacro content ()
  `(with-html
     (:div :id "content"
           (:table :class "listtable"
                   (:tbody
                    (:tr
                     (:td :colspan 2 :class "headline"
                          (i18n "imprint_headline")))
                    (:tr
                     (:td :colspan 2 :class "subline" " "))
                    (:tr :bgcolor "#646464"
                         (:td :colspan 2 :class "subline"
                              (i18n "imprint_subline")))
                    (:tr
                     (:td :colspan 2 " "))
                    (:tr
                     (:td :colspan 2
                          (:div :class "content"
                                "Manfred Bergmann" (:br)
                                "Burgbergweg 8" (:br)
                                "90559 Burgthann" (:br)
                                "Germany" (:br)
                                (:br)
                                "E-Mail: webmaster (at) software-by-mabe.com" (:br)
                                (:br)
                                (:raw (i18n "imprint_text"))))))))))

(defun render ()
  (log:debug "Rendering imprint view.")
  (page *page-title* (navigation) (content)))
