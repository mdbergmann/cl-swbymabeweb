(defpackage :cl-swbymabeweb.view.blog
  (:use :cl :blog :spinneret :cl-locale :view.common)
  (:nicknames :view.blog)
  (:export #:render
           #:blog-post
           #:blog-view-model))

(in-package :cl-swbymabeweb.view.blog)

(defparameter *page-title* "Manfred Bergmann | Software Development | Blog")

(defclass blog-post ()
  ((name :initform ""
         :type string
         :initarg :name)
   (date :initform "unknown"
         :type string
         :initarg :date)
   (text :initform ""
         :type string
         :initarg :text)))

(defclass blog-view-model ()
  ((blog-post :initform nil
              :initarg :blog-post)))

(defmacro blog-post-content (name date text)
  `(with-html
     (:div :class "blogLeftPanel"
           (:div :class "content_light" ,name)
           (:hr :class "blogtitle")
           (:div :class "content" ,date)
           (:div (:raw "&nbsp;"))
           (:div :class "content" ,text))))

(defmacro content (post)
  `(with-html
     (:div :id "content"
           (with-content-table
             (with-content-line
               ,post)))))

(defun render (view-model)
  (log:debug "Rendering blog view.")
  (with-slots (name date text) (slot-value view-model 'blog-post)
    (with-page *page-title*
      (content (blog-post-content name date text)))))
