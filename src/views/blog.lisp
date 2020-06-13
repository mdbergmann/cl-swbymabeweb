(defpackage :cl-swbymabeweb.view.blog
  (:use :cl :cl-who :str :cl-locale :blog-repo :view.common)
  (:nicknames :view.blog)
  (:export #:render
           #:blog-post-model
           #:blog-view-model
           #:blog-post
           #:all-blog-posts))

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
                   :reader model-get-all-posts)))

(defun blog-post-content (blog-post)
  (with-slots (name date text) blog-post
    (with-html-output-to-string (*standard-output* nil :prologue nil :indent t)
      (:div
            (:div :class "content_light" (str name))
            (:hr :class "blogtitle")
            (:div :class "content_tiny" (str date))
            (:div "&nbsp;")
            (:div :class "content blogLeftPanel" (str text))))))

(defun blog-post-navigation (blog-posts)
  (with-html-output-to-string (*standard-output* nil :prologue nil :indent t)
    (:div :class "content"
          (:ul :align "right"
               (dolist (elem blog-posts)
                 (str (blog-nav-entry elem)))))))

(defun name-to-link (blog-name)
  (format nil "/blog/~a"
          (str:replace-all " " "+" blog-name)))

(defun blog-nav-entry (post)
  (let* ((post-name (slot-value post 'name))
         (post-date (slot-value post 'nav-date))
         (post-link (name-to-link post-name)))
    (with-html-output-to-string (*standard-output* nil :prologue nil :indent t)
      (:li :class "blog-nav-item"
           (:a :href post-link
               :class "blog-nav-link"
               (str post-name))
           (:br)
           (:span :class "content_tiny" (str post-date))))))

(defun blog-header ()
  (with-html-output-to-string (*standard-output* nil :prologue nil :indent t)
    (with-content-table
      (content-headline (i18n "blog_headline")))))

(defun content (blog-post blog-navigation)
  (with-html-output-to-string (*standard-output* nil :prologue nil :indent t)
   (:div :id "content"
         (str (blog-header))
         (with-content-table
           (:tr
            (:td :class "blogLeftPanel"(str blog-post))
            (:td :class "blogNavPanel" (str blog-navigation)))))))

(defun render (view-model)
  (log:debug "Rendering blog view")
  (let ((blog-post (model-get-blog-post view-model))
        (all-posts (model-get-all-posts view-model)))
    (log:debug "post name: " (slot-value blog-post 'name))
    (log:debug "all-posts: " (length all-posts))
    (if blog-post
        (render-blog-post blog-post all-posts)
        (render-blog-post-empty all-posts))))

(defun render-blog-post-empty (all-posts)
  (with-page *page-title*
    (str (content nil
                  (blog-post-navigation all-posts)))))

(defun render-blog-post (blog-post all-posts)
  (with-page *page-title*
    (str (content (blog-post-content blog-post)
                  (blog-post-navigation all-posts)))))
