(defpackage :cl-swbymabeweb.view.common
  (:use :cl :spinneret :cl-locale)
  (:nicknames :view.common)
  (:export #:page
           #:navigation))

(in-package :cl-swbymabeweb.view.common)

(defmacro nav-entry-separator ()
  `(with-html
     (:td :width "25" "|")))

(defmacro nav-entry-blog ()
  `(with-html
     (:td :width "70"
          (:a :href "/blog" (i18n "blog")))))

(defmacro nav-entry-about ()
  `(with-html
     (:td :width "70"
          (:a :href "/about" (i18n "about")))))

(defmacro nav-entry-imprint ()
  `(with-html
     (:td :width "80"
          (:a :href "/imprint" (i18n "imprint")))))

(defmacro navigation ()
  `(with-html
     (:div :id "navigation"
           (:table :class "mainnav"
                   (:tbody
                    (:tr :class "navi_boldwhite"
                         (nav-entry-blog)
                         (nav-entry-separator)
                         (nav-entry-about)
                         (nav-entry-separator)
                         (nav-entry-imprint)))))))

(defmacro page (title navigation content)
  (let* ((max-width 1024)
         (logo-width 220)
         (logo-col-width 228)
         (nav-col-width (- max-width logo-col-width))
         (top-height 55)
         (logo-height 30)
         (bottom-height 70))
    `(with-html-string
       (:doctype)
       (:html
        (:head
         (:title ,title)
         (:link :rel "stylesheet" :href "css/formate.css")
         (:meta :http-equiv "Content-Type"
                :content "text/html; charset=utf-8"))
        (:body
         (:table :class "listtable"
                 (:tbody
                  (:tr
                   (:td :height ,top-height :width ,logo-col-width " ")
                   (:td :width ,nav-col-width " "))
                  (:tr
                   (:td
                    (:a :href "#"
                        (:img :src "gfx/logo.gif"
                              :alt "back to home"
                              :border "0"
                              :height ,logo-height :width ,logo-width)))
                   (:td :valign "bottom"
                        ,navigation))
                  (:tr
                   (:td :height ,bottom-height " ")
                   (:td " "))))
         )))))
