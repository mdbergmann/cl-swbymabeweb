(defpackage :cl-swbymabeweb.controller.blog
  (:use :cl :blog-repo :controller :view.blog :local-time)
  (:nicknames :controller.blog)
  (:export #:index
           #:for-blog-name
           #:atom-feed)
  (:import-from #:cl-swbymabeweb.config
                #:config)
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

(-> atom-feed () controller-result)
(defun atom-feed ()
  "Call the atom feed generation."
  (let ((lookup-result (repo-get-all)))
    (case (car lookup-result)
      (:ok
       (progn
         (let ((feed-result
                 (atom-feed:generate-feed (make-feed-model (cdr lookup-result)))))
           (case (car feed-result)
             (:ok
              (make-controller-result
               :ok
               (cdr feed-result)))
             (t
              (make-controller-result
               (car feed-result)
               (cdr feed-result)))))))
      (t
       (make-controller-result
        (car lookup-result)
        (cdr lookup-result))))))

(defun make-feed-model (blog-entries)
  (atom-feed:make-atom-feed-model
   (atom-feed:make-atom-feed-header :title "Manfred Bergmann's blog"
                                    :author "Manfred Bergmann / Software by MaBe"
                                    :link (format nil "~a/" (config :web-url-base))
                                    :updated (if (car blog-entries)
                                                 (blog-date-to-atom-timestring (car blog-entries)))
                                    :id (format nil "~a/" (config :web-url-base)))
   (mapcar #'make-feed-entry blog-entries)))

(defun make-feed-entry (blog-entry)
  (atom-feed:make-atom-feed-entry
   :title (blog-entry-name blog-entry)
   :link (blog-name-to-atom-link (config :web-url-base)
                                 (blog-entry-name blog-entry))
   :updated (blog-date-to-atom-timestring blog-entry)
   :id (blog-name-to-atom-link (config :web-url-base)
                               (blog-entry-name blog-entry))
   :content (blog-entry-text blog-entry)))

(defun blog-name-to-atom-link (base-url blog-name)
  (format nil "~a/blog/~a"
          base-url
          (str:replace-all " " "+" blog-name)))

(defun blog-date-to-atom-timestring (blog-entry)
  (format-timestring nil
                     (universal-to-timestamp (blog-entry-date blog-entry))
                     :format atom-feed:+atom-datetime-format+))
