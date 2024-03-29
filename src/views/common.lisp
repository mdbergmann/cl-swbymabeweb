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

(defmacro header-navigation ()
  `(htm
    (:a :href "/blog" (fmt "[~a]" #!"blog-top-nav"))
    (str "|")
    (:a :href "/projects" (fmt "[~a]" #!"projects-top-nav"))
    (str "|")
    (:a :href "/about" (fmt "[~a]" #!"about-top-nav"))
    (str "|")
    (:a :href "/imprint" (fmt "[~a]" #!"imprint-top-nav"))))

(defmacro page-header (header-navigation)
  `(htm
    (:div :class "header_logo" (str "Manfred Bergmann"))
    (:div :class "header_nav" ,header-navigation)
    (:div :class "header_line" (:hr))))

(defmacro page-footer ()
  `(htm
    (:div :class "footer_line" (:hr))
    (:div :class "footer_left" (str #!"all_copyright"))
    (:div :class "footer_right" (str
                                 (format-timestring
                                  nil (now)
                                  :format +asctime-format+)))))

(defmacro with-page ((title header-style) &rest body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html
      (:head
       (:title (str ,title))
       (:link :rel "stylesheet" :href "/static/css/formate.css")
       (:link :rel "stylesheet" :href "/static/js/styles/my-owl.css")
       (:script :src "/static/js/highlight.min.js")
       (:script (str "hljs.highlightAll();"))
       (:meta :http-equiv "Content-Type"
              :content "text/html; charset=utf-8")
       (:style (str ,header-style)))
      (:body
       (:div :class "wrapper"
             (page-header (header-navigation))
             (:div :class "content"
                   ,@body)
             (page-footer))))))
