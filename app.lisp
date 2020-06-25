(ql:quickload :cl-swbymabeweb)

(defpackage cl-swbymabeweb.app
  (:use :cl :log4cl)
  (:import-from #:lack.builder
                #:builder)
  (:import-from #:ppcre
                #:scan
                #:regex-replace)
  (:import-from #:cl-swbymabeweb.web
                #:*web*)
  (:import-from #:cl-swbymabeweb.config
                #:config
                #:productionp
                #:*static-directory*
                #:*logs-directory*))
(in-package :cl-swbymabeweb.app)

(log:config :info :sane :daily "logs/app.log" :backup nil)

(builder
 (:static
  :path (lambda (path)
          (if (ppcre:scan "^(?:/gfx/|/css/|/js/|/download/|/robot\\.txt$|/favicon\\.ico$)" path)
              path
              nil))
  :root *static-directory*)
 (:accesslog
  :logger (lambda (message)
            (str:to-file
             (merge-pathnames #P"access.log" *logs-directory*)
             (format nil "~a~%" message)
             :if-exists :append
             :if-does-not-exist :create)))
 *web*)
