(in-package :cl-user)
(defpackage cl-swbymabeweb
  (:use :cl :log4cl)
  (:import-from :cl-swbymabeweb.config
                :config)
  (:import-from :clack
                :clackup)
  (:export :start
           :stop))
(in-package :cl-swbymabeweb)

(defvar *approot-path*
  (asdf:system-relative-pathname :cl-swbymabeweb #P""))

(defvar *appfile-path*
  (merge-pathnames #P"app.lisp" *approot-path*))

(cl-locale:enable-locale-syntax)
(cl-locale:define-dictionary default
  (:en_EN (merge-pathnames #P"i18n/en_EN/default.lisp" *approot-path*)))
(setf (cl-locale:current-dictionary) :default)
(setf cl-locale:*locale* :en_EN)

(defvar *handler* nil)

(defun start (&rest args &key server port debug &allow-other-keys)
  (declare (ignore server port debug))
  (log:info "Starting server.")
  (when *handler*
    (log:info "Server is already running."))
  (unless *handler*
    (setf *handler*
          (apply #'clackup *appfile-path* args))))

(defun stop ()
  (when *handler*
    (log:info "Stopping server.")
    (prog1
        (clack:stop *handler*)
      (log:debug "Server stopped.")
      (setf *handler* nil))))
