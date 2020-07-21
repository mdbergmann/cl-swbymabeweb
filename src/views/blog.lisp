(defpackage :cl-swbymabeweb.view.blog
  (:use :cl :cl-who :str :cl-i18n :blog-repo :view.common)
  (:nicknames :view.blog)
  (:export #:render
           #:blog-post-model
           #:blog-view-model))

(in-package :cl-swbymabeweb.view.blog)

(defparameter *page-title* "Manfred Bergmann | Software Development | Blog")

(defclass blog-post-model ()
  ((name :initform ""
         :type string
         :initarg :name)
   (date :initform ""
         :type string
         :initarg :date)
   (nav-date :initform ""
             :type string
             :initarg :nav-date)
   (text :initform ""
         :type string
         :initarg :text)))

(defclass blog-view-model ()
  ((blog-post :initform nil
              :initarg :blog-post
              :reader model-get-blog-post)
   (all-blog-posts :initform '()
                   :initarg :all-blog-posts
                   :reader model-get-all-posts)
   (atom-url :initform nil
             :initarg :atom-url
             :reader model-get-atom-url)))

(defmacro blog-post-content (blog-post)
  (let ((post (gensym))
        (name (gensym))
        (date (gensym))
        (text (gensym)))
    `(let* ((,post ,blog-post)
            (,name (slot-value ,post 'name))
            (,date (slot-value ,post 'date))
            (,text (slot-value ,post 'text)))
       (htm
        (:div
         (:div :class "content-light" (str ,name))
         (:hr :class "blog-title")
         (:div :class "content-tiny" (str ,date))
         (:div "&nbsp;")
         (:div :class "content blog-left-panel" (str ,text)))))))

(defmacro blog-post-navigation (view-model)
  (let ((elem (gensym)))
    `(htm
      (:div :class "content"
            (:div :style "text-align: center;"
                  (:a :href (model-get-atom-url ,view-model)
                      :class "blog-nav-link"
                      (str "atom/rss feed")))
            (:p "&nbsp;")
            (:ul :align "right"
                 (dolist (,elem (model-get-all-posts ,view-model))
                   (blog-nav-entry ,elem)))))))

(defun name-to-link (blog-name)
  (format nil "/blog/~a"
          (str:replace-all " " "+" blog-name)))

(defmacro blog-nav-entry (blog-post)
  (let ((post (gensym))
        (name (gensym))
        (nav-date (gensym))
        (link (gensym)))
    `(let* ((,post ,blog-post)
            (,name (slot-value ,post 'name))
            (,nav-date (slot-value ,post 'nav-date))
            (,link (name-to-link ,name)))
       (htm
        (:li :class "blog-nav-item"
             (:a :href ,link
                 :class "blog-nav-link"
                 (str ,name))
             (:br)
             (:span :class "content-tiny" (str ,nav-date)))))))

(defmacro blog-header ()
  `(htm
    (with-content-table
      (content-headline #!"blog_headline"))))

(defmacro content (blog-post blog-navigation)
  `(htm
    (:div :id "content"
          (str (blog-header))
          (with-content-table
            (:tr
             (:td :class "blog-left-panel"(str ,blog-post))
             (:td :class "blog-nav-panel" (str ,blog-navigation)))))))

(defun render (view-model)
  (log:debug "Rendering blog view")
  (let ((blog-post (model-get-blog-post view-model)))
    (log:debug "post name: " (slot-value blog-post 'name))

    (with-page *page-title*
      (str (content (if blog-post
                        (blog-post-content blog-post)
                        nil)
                    (blog-post-navigation view-model))))))
