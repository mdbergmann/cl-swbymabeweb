(defpackage :cl-swbymabeweb.view.common
  (:use :cl :spinneret)
  (:nicknames :view.common)
  (:export #:page))

(in-package :cl-swbymabeweb.view.common)

(defmacro page (title navigation content)
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
                 (:td :height "55" :width "228" " ")
                 (:td :width "572" " "))
                (:tr
                 (:td
                  (:a :href "#"
                      (:img :src "gfx/logo.gif"
                            :alt "back to home"
                            :border "0"
                            :height "30" :width "220")))
                 (:td :valign "bottom"
                      ,navigation))
                (:tr
                 (:td :height "70" " ")
                 (:td " "))))
       ))))
