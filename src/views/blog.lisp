(defpackage :cl-swbymabeweb.view.blog
  (:use :cl :cl-who :str :cl-i18n :view.common)
  (:nicknames :view.blog)
  (:export #:render
           #:blog-post-model
           #:blog-view-model)
  (:import-from #:alexandria
                #:with-gensyms))

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

(defmacro blog-post-article (blog-post)
  (with-gensyms (post name date text)
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
  (with-gensyms (elem)
    `(htm
      (:div :style "text-align: center;"
             (:a :href (model-get-atom-url ,view-model)
                 (str "[atom/rss feed]")))
       (:p "&nbsp;")
       (:ul
        (dolist (,elem (model-get-all-posts ,view-model))
          (htm (blog-nav-entry ,elem)))))))

(defmacro blog-nav-entry (blog-post)
  (with-gensyms (post name nav-date link)
    `(let* ((,post ,blog-post)
            (,name (slot-value ,post 'name))
            (,nav-date (slot-value ,post 'nav-date))
            (,link (name-to-link ,name)))
       (htm
        (:li
         (:a :href ,link (fmt "[~a]" ,name))
         (:br)
         (:date (str ,nav-date)))))))

(defun name-to-link (blog-name)
  (format nil "/blog/~a"
          (str:replace-all " " "+" blog-name)))

(defun render (view-model)
  (log:debug "Rendering blog view")
  (let ((blog-post (model-get-blog-post view-model)))
    (log:debug "post name: " (slot-value blog-post 'name))

    (with-page (*page-title*
                (htm
                 "
.sub_content {
    width: 100%;
    display: grid;
    gap: 10px;
    grid-template-areas: '1st 2nd';
    grid-template-columns: fit-content(68%) 1fr;
    grid-template-rows: auto;
}
.article {
    grid-area: 1st;
}
.article date {
    font-size: 11pt;
}
.recent_articles {
    grid-area: 2nd;
    text-align: left;
    font-size: 11pt;
}
.recent_articles date {
    font-size: 9pt;
    color: var(--darker-color);
}
"))
       (:div :class "sub_content"
             (:div :class "article" (if blog-post
                                        (htm
                                         (blog-post-article blog-post))
                                        nil))
             (:div :class "recent_articles" (htm
                                             (blog-recent-articles-nav view-model)))))))
