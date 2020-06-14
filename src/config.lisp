(in-package :cl-user)
(defpackage cl-swbymabeweb.config
  (:use :cl)
  (:import-from #:envy
                #:config-env-var
                #:defconfig)
  (:export #:config
           #:*application-root*
           #:*static-directory*
           #:*template-directory*
           #:*blog-folder-path*
           #:appenv
           #:developmentp
           #:productionp))
(in-package :cl-swbymabeweb.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root* (asdf:system-source-directory :cl-swbymabeweb))
(defparameter *static-directory* (merge-pathnames #P"static/" *application-root*))
(defparameter *blog-folder-path* (merge-pathnames #P"blogs/" *application-root*))

(defconfig :common
    `(:locale :en_EN))

(defconfig |development|
    '())

(defconfig |production|
    '())

(defconfig |test|
    '())

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
