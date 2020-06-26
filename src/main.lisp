(in-package :cl-user)
(defpackage cl-swbymabeweb
  (:use :cl :log4cl)
  (:import-from #:cl-swbymabeweb.web
                #:make-routes)
  (:import-from #:cl-swbymabeweb.config
                #:config
                #:*application-root*
                #:*blog-directory*
                #:*static-directory*
                #:*logs-directory*)
  (:import-from #:blog-repo
                #:blog-repo-fac-init
                #:blog-repo-default)
  (:export :start
           :stop))
(in-package :cl-swbymabeweb)

(cl-locale:enable-locale-syntax)
(cl-locale:define-dictionary default
  (:en_EN (merge-pathnames #P"i18n/en_EN/default.lisp" *application-root*)))
(setf (cl-locale:current-dictionary) :default)
(setf cl-locale:*locale* (config :locale))

(defvar *server* nil)

(defun start (&key debug (port 5000) (address "0.0.0.0") &allow-other-keys)
  (declare (ignore debug))

  (log:info "Initializing blog-repo factory.")
  (blog-repo-fac-init (make-instance 'blog-repo-default :blog-folder *blog-directory*))

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
          (make-instance 'hunchentoot:easy-acceptor
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

