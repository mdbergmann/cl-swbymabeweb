(defpackage cl-swbymabeweb.web
  (:use :cl :snooze :cl-swbymabeweb.config)
  (:export #:make-routes))
(in-package :cl-swbymabeweb.web)

;;
;; Application

(defun make-routes ()
  (make-hunchentoot-app))

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
      (t (http-condition 400 "Undefined error!")))))

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
