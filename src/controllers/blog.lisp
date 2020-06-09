(defpackage :cl-swbymabeweb.controller.blog
  (:use :cl :blog-repo :controller :view.blog :local-time)
  (:nicknames :controller.blog)
  (:export #:index
           #:for-blog-name)
  (:import-from #:serapeum
                #:->))

(in-package :cl-swbymabeweb.controller.blog)

(-> index () controller-result)
(defun index ()
  "`index' looks up the latest blog entry and delivers it to the view."
  (log:debug "")

  (let* ((lookup-result (blog-repo:repo-get-latest))
         (view-model (make-instance 'blog-view-model
                                    :blog-post (blog-entry-to-blog-post (cdr lookup-result)))))  
    (cons :ok (view.blog:render view-model))))

(-> for-blog-name (simple-base-string) controller-result)
(defun for-blog-name (name)
  "`for-blog-name' looks up a blog entry a blog entry for the given name
and delivers it to the view for rendering.
If the blog entry with that name doesn't exist the controller will report 
an error to the caller which the caller is responsible to handle."
  (log:debug "blog name: " name)

  (let ((lookup-result (blog-repo:repo-get-blog-entry name)))
    (if (eq :ok (car lookup-result))
        (cons :ok (view.blog:render
                   (make-instance 'blog-view-model
                                  :blog-post (blog-entry-to-blog-post (cdr lookup-result)))))
        lookup-result)))

(defun blog-entry-to-blog-post (blog-entry)
  "Converts `blog-entry' to `blog-post'.
This function makes a mapping from the repositotry blog entry to the view model blog entry."
  (when blog-entry
    (make-instance 'blog-post
                   :name (blog-entry-name blog-entry)
                   :date (format-timestring nil
                                            (blog-entry-date blog-entry)
                                            :format +asctime-format+)
                   :text (blog-entry-text blog-entry))))
