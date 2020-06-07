(defpackage :cl-swbymabeweb.controller.blog
  (:use :cl :view.blog :blog :local-time)
  (:nicknames :controller.blog)
  (:export #:index))

(in-package :cl-swbymabeweb.controller.blog)

(defun index ()
  (log:debug "Blog controller.")

  (let ((view-model (make-instance 'blog-view-model
                                   :blog-post (blog-entry-to-blog-post (blog:get-latest)))))  
    (view.blog:render view-model)))

(defun blog-entry-to-blog-post (blog-entry)
  (make-instance 'blog-post
                 :name (get-entry-name blog-entry)
                 :date (format-timestring nil
                                          (get-entry-date blog-entry)
                                          :format +asctime-format+)
                 :text (get-entry-text blog-entry)))
