(defpackage :cl-swbymabeweb.view.common
  (:use :cl :cl-who :cl-i18n :local-time)
  (:nicknames :view.common)
  (:export #:with-page
           #:with-content-table
           #:with-content-line
           #:content-headline
           #:content-subline))

(in-package :cl-swbymabeweb.view.common)

;; this is our mode
(setf (html-mode) :sgml)

(defmacro with-content-table (&body body)
  `(htm
    (:table :class "list-table"
            (:tbody
             ,@body))))

(defmacro content-headline (title)
  `(htm
    (:tr
     (:td :colspan 2 :class "headline" (str ,title)))
    (:tr
     (:td :colspan 2 :class "subline" "&nbsp;"))))

(defmacro content-subline (title)
  `(htm
    (:tr :bgcolor "#646464"
         (:td :colspan 2 :class "subline" (str ,title)))))

(defmacro with-content-line (&body body)
  `(htm
    (:tr
     (:td :colspan 2 :class "content" ,@body))
    (:tr
     (:td :colspan 2 "&nbsp;"))))

(defmacro nav-entry-separator ()
  `(htm
    (:td :width "25" "|")))

(defmacro nav-entry (width link text)
  `(htm
    (:td :width ,width
         (:a :href ,link (str ,text)))))

(defmacro navigation ()
  `(htm
    (:div :id "navigation"
          (:table :class "main-nav"
                  (:tbody
                   (:tr :class "navi_boldwhite"
                        (nav-entry "70" "/blog" #!"blog")
                        (nav-entry-separator)
                        (nav-entry "70" "/about" #!"about")
                        (nav-entry-separator)
                        (nav-entry "80" "/imprint" #!"imprint")))))))

(defmacro page-header (navigation)
  (let* ((max-width 1024)
         (logo-width 220)
         (logo-col-width 228)
         (nav-col-width (- max-width logo-col-width))
         (top-height 55)
         (logo-height 30)
         (bottom-height 70))
    `(htm
      (:table :class "list-table"
              (:tbody
               (:tr
                (:td :height ,top-height :width ,logo-col-width "&nbsp;")
                (:td :width ,nav-col-width "&nbsp;"))
               (:tr
                (:td
                 (:a :href "/"
                     (:img :src "/static/gfx/logo.gif"
                           :alt "back to home"
                           :border "0"
                           :height ,logo-height :width ,logo-width)))
                (:td :valign "bottom"
                     ,navigation))
               (:tr
                (:td :height ,bottom-height "&nbsp;")
                (:td "&nbsp;")))))))

(defmacro page-footer ()
  `(htm
    (with-content-table
      (:tr
       (:td :colspan 2 "&nbsp;"))
      (:tr
       (:td :colspan 2 (:hr)))
      (:tr
       (:td :class "content-light content-small"
            (str #!"all_copyright"))
       (:td :class "content-light content-small"
            (:div :align "right"
                  (str (format-timestring nil (now)
                                          :format +asctime-format+)))))
      (:tr
       (:td :colspan 2 "&nbsp;")))))

(defmacro with-page (title &rest body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html
      (:head
       (:title (str ,title))
       (:link :rel "stylesheet" :href "/static/css/formate.css")
       (:meta :http-equiv "Content-Type"
              :content "text/html; charset=utf-8"))
      (:body
       (page-header (navigation))
       ,@body
       (page-footer)))))
