(in-package :cl-user)
(defpackage cl-swbymabeweb
  (:use :cl :log4cl)
  (:import-from #:cl-swbymabeweb.config
                #:config
                #:*application-root*
                #:*blog-directory*)
  (:import-from #:clack
                #:clackup)
  (:import-from #:blog-repo
                #:blog-repo-fac-init
                #:blog-repo-default)
  (:export :start
           :stop))
(in-package :cl-swbymabeweb)

(defvar *appfile-path* (merge-pathnames #P"app.lisp" *application-root*))

(cl-locale:enable-locale-syntax)
(cl-locale:define-dictionary default
  (:en_EN (merge-pathnames #P"i18n/en_EN/default.lisp" *application-root*)))
(setf (cl-locale:current-dictionary) :default)
(setf cl-locale:*locale* (config :locale))

(defvar *web-handler* nil)

(defun start (&rest args &key server port debug &allow-other-keys)
  (declare (ignore server port debug))

  (log:info "Initializing blog-repo factory.")
  (blog-repo-fac-init (make-instance 'blog-repo-default :blog-folder *blog-directory*))

  (log:info "Starting server.")
  (when *web-handler*
    (log:info "Server is already running."))
  (unless *web-handler*
    (setf *web-handler*
          (apply #'clackup *appfile-path* args))))

(defun stop ()
  (when *web-handler*
    (log:info "Stopping server.")
    (prog1
        (clack:stop *web-handler*)
      (log:debug "Server stopped.")
      (setf *web-handler* nil))))

