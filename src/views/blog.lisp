(defpackage :cl-swbymabeweb.view.blog
  (:use :cl :cl-who :str :cl-i18n :view.common)
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

(defun render (view-model)
  (log:debug "Rendering blog view")
  (let ((blog-post (model-get-blog-post view-model)))
    (log:debug "post name: " (slot-value blog-post 'name))

    (with-page *page-title*
      (str (content (if blog-post
                        (blog-post-article blog-post)
                        nil)
                    (blog-recent-articles-nav view-model))))))

(defmacro content (blog-post blog-navigation)
  `(htm
    (:div :class "article" (str ,blog-post))
    (:div :class "recent_articles" (str ,blog-navigation))))

(defmacro blog-post-article (blog-post)
  (let ((post (gensym))
        (name (gensym))
        (date (gensym))
        (text (gensym)))
    `(let* ((,post ,blog-post)
            (,name (slot-value ,post 'name))
            (,date (slot-value ,post 'date))
            (,text (slot-value ,post 'text)))
       (htm
        (:h4 (str ,name))
        (:date (str ,date))
        (:div "&nbsp;")
        (:div (str ,text))))))

(defmacro blog-recent-articles-nav (view-model)
  (let ((elem (gensym)))
    `(htm
      (:div :style "text-align: center;"
            (:a :href (model-get-atom-url ,view-model)
                (str "[atom/rss feed]")))
      (:p "&nbsp;")
      (:ul
       (dolist (,elem (model-get-all-posts ,view-model))
         (blog-nav-entry ,elem))))))

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
        (:li
         (:a :href ,link
             :class "blog-nav-link"
             (str (format nil "[~a]" ,name)))
         (:br)
         (:date (str ,nav-date)))))))

(defun name-to-link (blog-name)
  (format nil "/blog/~a"
          (str:replace-all " " "+" blog-name)))
