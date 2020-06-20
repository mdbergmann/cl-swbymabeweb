(ql:quickload :cl-swbymabeweb)

(defpackage cl-swbymabeweb.app
  (:use :cl :log4cl)
  (:import-from :lack.builder
                :builder)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :cl-swbymabeweb.web
                :*web*)
  (:import-from :cl-swbymabeweb.config
                :config
                :productionp
                :*static-directory*))
(in-package :cl-swbymabeweb.app)

(log:config :info :daily "access-log/access.log" :backup nil)

(builder
 (:static
  :path (lambda (path)
          (if (ppcre:scan "^(?:/gfx/|/css/|/js/|/download/|/robot\\.txt$|/favicon\\.ico$)" path)
              path
              nil))
  :root *static-directory*)
 (:accesslog
  :logger (lambda (message)
            (log:info message)))
 *web*)
