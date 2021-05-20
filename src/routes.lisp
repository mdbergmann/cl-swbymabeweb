(defpackage cl-swbymabeweb.routes
  (:use :cl :snooze :cl-swbymabeweb.config)
  (:export #:make-routes))
(in-package :cl-swbymabeweb.routes)

;;
;; Application

(defun make-routes ()
  (make-hunchentoot-app))

(defun my-default-resource-name (uri)
  (when (or (string= "" uri)
            (string= "/" uri))
    (return-from my-default-resource-name (values nil nil)))
  (let* ((first-slash-or-qmark (position-if #'(lambda (char)
                                                (member char '(#\/ #\?)))
                                            uri
                                            :start 1)))
    (values (cond (first-slash-or-qmark
                   (subseq uri 1 first-slash-or-qmark))
                  (t
                   (subseq uri 1)))
            (if first-slash-or-qmark
                (subseq uri first-slash-or-qmark)))))

(setf *resource-name-function* #'my-default-resource-name)

;;
;; Routing rules

(defroute home (:get :text/html)
  (log:debug "Index route called.")
  (cdr (controller.index:index)))

(defroute imprint (:get :text/html)
  (log:debug "Imprint route called.")
  (cdr (controller.imprint:index)))

(defroute about (:get :text/html)
  (log:debug "About route called.")
  (cdr (controller.about:index)))

(defroute projects (:get :text/html)
  (log:debug "Projects route called.")
  (cdr (controller.projects:index)))

(defroute blog (:get :text/html &optional name)
  (log:debug "Blog route called with name: " name)
  (cond
    ((null name) (blog-index-handler))
    (t (blog-by-name-handler (string name)))))

(defun blog-index-handler ()
  (log:debug "Blog route called.")
  (let ((result (controller.blog:index)))
    (case (car result)
      (:ok (cdr result))
      (t (http-condition 500 "Undefined error!")))))

(defun blog-by-name-handler (name)
  (let ((result (controller.blog:for-blog-name (plus-to-space name))))
    (case (car result)
      (:ok (cdr result))
      (:not-found-error (http-condition 404 (cdr result)))
      (t (http-condition 400 "Undefined error!")))))

(defun plus-to-space (text)
  (str:replace-all "+" " " text))

(defroute blog-atom-feed (:get "application/xml")
  (log:debug "Blog route for atom feed")
  (let ((result (controller.blog:atom-feed)))
    (case (car result)
      (:ok (cdr result))
      (t (http-condition 400 "Undefined error!")))))
