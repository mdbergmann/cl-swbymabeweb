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
  (let ((lookup-result (blog-repo:repo-get-latest))
        (all-posts-result (blog-repo:repo-get-all)))
    (make-controller-result
     :ok
     (view.blog:render
      (make-view-model (cdr lookup-result) (cdr all-posts-result))))))

(-> for-blog-name (string) controller-result)
(defun for-blog-name (name)
  "`for-blog-name' looks up a blog entry a blog entry for the given name
and delivers it to the view for rendering.
If the blog entry with that name doesn't exist the controller will report 
an error to the caller which the caller is responsible to handle."  
  (log:debug "blog name: " name)
  (let ((lookup-result (blog-repo:repo-get-for-name name)))
    (if (eq :ok (car lookup-result))
        (make-controller-result
         :ok
         (view.blog:render
          (make-view-model (cdr lookup-result) (cdr (blog-repo:repo-get-all)))))
        (make-controller-result
         (car lookup-result)
         (cdr lookup-result)))))

(defun make-controller-result (first second)
  "Convenience function to create a new controller result.
But also used for visibility of where the result is created."
  (cons first second))

(defun make-view-model (the-blog-entry all-posts)
  (make-instance 'blog-view-model
                 :blog-post
                 (blog-entry-to-blog-post the-blog-entry)
                 :all-blog-posts
                 (mapcar #'blog-entry-to-blog-post all-posts)))

(defun blog-entry-to-blog-post (blog-entry)
  "Converts `blog-entry' to `blog-post'.
This function makes a mapping from the repositotry blog entry to the view model blog entry."
  (log:debug "Converting post: " blog-entry)
  (when blog-entry
    (make-instance 'blog-post-model
                   :name (blog-entry-name blog-entry)
                   :date (format-timestring nil
                                            (universal-to-timestamp (blog-entry-date blog-entry))
                                            :format '((:day 2) #\Space  :long-month #\Space (:year 4)))
                   :nav-date (format-timestring nil
                                                (universal-to-timestamp (blog-entry-date blog-entry))
                                                :format '((:day 2) #\- (:month 2) #\- (:year 4)))
                   :text (blog-entry-text blog-entry))))
