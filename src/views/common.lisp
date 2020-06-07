(defpackage :cl-swbymabeweb.view.common
  (:use :cl :spinneret :cl-locale :local-time)
  (:nicknames :view.common)
  (:export #:with-page
           #:with-content-table
           #:with-content-line
           #:content-headline
           #:content-subline))

(in-package :cl-swbymabeweb.view.common)

(defmacro with-content-table (&body body)
  `(with-html
     (:table :class "listtable"
             (:tbody
              ,@body))))

(defmacro content-headline (title)
  `(with-html
     (:tr
      (:td :colspan 2 :class "headline" ,title))
     (:tr
      (:td :colspan 2 :class "subline" (:raw "&nbsp;")))))

(defmacro content-subline (title)
  `(with-html
     (:tr :bgcolor "#646464"
      (:td :colspan 2 :class "subline" ,title))))

(defmacro with-content-line (&body body)
  `(with-html
     (:tr
      (:td :colspan 2 :class "content" ,@body))
     (:tr
      (:td :colspan 2 (:raw "&nbsp;")))))

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

(defmacro page-header (navigation)
  (let* ((max-width 1024)
         (logo-width 220)
         (logo-col-width 228)
         (nav-col-width (- max-width logo-col-width))
         (top-height 55)
         (logo-height 30)
         (bottom-height 70))
    `(with-html
       (:table :class "listtable"
               (:tbody
                (:tr
                 (:td :height ,top-height :width ,logo-col-width (:raw "&nbsp;"))
                 (:td :width ,nav-col-width (:raw "&nbsp;")))
                (:tr
                 (:td
                  (:a :href "/"
                      (:img :src "gfx/logo.gif"
                            :alt "back to home"
                            :border "0"
                            :height ,logo-height :width ,logo-width)))
                 (:td :valign "bottom"
                      ,navigation))
                (:tr
                 (:td :height ,bottom-height (:raw "&nbsp;"))
                 (:td (:raw "&nbsp;"))))))))

(defmacro page-footer ()
  `(with-html
     (with-content-table
         (:tr
          (:td :colspan 2 (:raw "&nbsp;")))
       (:tr
        (:td :colspan 2 (:hr)))
       (:tr
        (:td :class "content_light"
             (i18n "all_copyright"))
        (:td :class "content_light"
             (:div :align "right"
                   (format-timestring nil (now)
                                      :format +asctime-format+))))
       (:tr
        (:td :colspan 2 (:raw "&nbsp;"))))))

(defmacro with-page (title &rest body)
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title)
       (:link :rel "stylesheet" :href "css/formate.css")
       (:meta :http-equiv "Content-Type"
              :content "text/html; charset=utf-8"))
      (:body
       (page-header (navigation))
       ,@body
       (page-footer)
       ))))
