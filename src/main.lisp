(in-package :cl-user)
(defpackage cl-swbymabeweb
  (:use :cl :log4cl)
  (:import-from #:cl-swbymabeweb.routes
                #:make-routes)
  (:import-from #:cl-swbymabeweb.config
                #:config
                #:*application-root*
                #:*blog-directory*
                #:*static-directory*
                #:*logs-directory*)
  (:import-from #:blog-repo
                #:*blog-repo*
                #:blog-repo-default)
  (:export #:start
           #:stop
           #:re-init-blog-repo))
(in-package :cl-swbymabeweb)

(setf cl-i18n:*translation-file-root*
      (merge-pathnames #P"i18n/" *application-root*))
(cl-i18n:load-language "english.lisp")

(defvar *server* nil)

(defclass app-acceptor (hunchentoot:easy-acceptor) ()
  (:documentation "Acceptor that rejects HEAD requests and hides the Server header."))

(defmethod hunchentoot:acceptor-dispatch-request :around ((acceptor app-acceptor) request)
  ;; an explicit NIL makes Hunchentoot omit the Server header
  (setf (hunchentoot:header-out :server) nil)
  (if (eq (hunchentoot:request-method request) :head)
      (progn
        (setf (hunchentoot:header-out :allow) "GET")
        (setf (hunchentoot:return-code*) hunchentoot:+http-method-not-allowed+)
        nil)
      (call-next-method)))

(defmethod hunchentoot:acceptor-status-message ((acceptor app-acceptor) http-status-code
                                                &key &allow-other-keys)
  ;; plain error page, without Hunchentoot's server/version footer
  (when (<= 300 http-status-code)
    (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
    (let ((status (format nil "~D ~A" http-status-code
                          (hunchentoot:reason-phrase http-status-code))))
      (format nil "<html><head><title>~A</title></head><body><h1>~A</h1></body></html>"
              status status))))

(defun start (&key debug (port 5000) (address "0.0.0.0") &allow-other-keys)
  (declare (ignore debug))

  (log:info "Initializing blog-repo instance.")
  (setf *blog-repo* (make-instance 'blog-repo-default :blog-folder *blog-directory*))

  (log:info "Starting server.")
  (when *server*
    (log:info "Server is already running."))
  (unless *server*

    ;; order is important
    (push (make-routes)
          hunchentoot:*dispatch-table*)
    (push (hunchentoot:create-folder-dispatcher-and-handler "/static/" *static-directory*)
          hunchentoot:*dispatch-table*)

    (setf *server*
          (make-instance 'app-acceptor
                         :port port
                         :address address
                         :access-log-destination
                         (merge-pathnames "access.log" *logs-directory*)
                         :message-log-destination
                         (merge-pathnames "message.log" *logs-directory*)))
    
    (hunchentoot:start *server*)))

(defun stop ()
  (when *server*
    (log:info "Stopping server.")
    (prog1
        (hunchentoot:stop *server*)
      (log:debug "Server stopped.")
      (setf hunchentoot:*dispatch-table* nil)
      (setf *server* nil))))

(defun re-init-blog-repo ()
  (blog-repo::blog-agent-reinit))
