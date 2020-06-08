(defpackage :cl-swbymabeweb.controller.blog
  (:use :cl :view.blog :blog :local-time)
  (:nicknames :controller.blog)
  (:export #:index
           #:for-blog-name))

(in-package :cl-swbymabeweb.controller.blog)

(defun index ()
  (log:debug "")

  (let* ((lookup-result (blog:get-latest))
         (view-model (make-instance 'blog-view-model
                                    :blog-post (blog-entry-to-blog-post (cdr lookup-result)))))  
    (cons :ok (view.blog:render view-model))))

(defun for-blog-name (name)
  (log:debug "blog name: " name)

  (let ((lookup-result (blog:get-blog-entry name)))
    (if (eq :ok (car lookup-result))
        (cons :ok (view.blog:render
                   (make-instance 'blog-view-model
                                  :blog-post (blog-entry-to-blog-post (cdr lookup-result)))))
        lookup-result)))

(defun blog-entry-to-blog-post (blog-entry)
  (when blog-entry
    (make-instance 'blog-post
                   :name (get-entry-name blog-entry)
                   :date (format-timestring nil
                                            (get-entry-date blog-entry)
                                            :format +asctime-format+)
                   :text (get-entry-text blog-entry))))
