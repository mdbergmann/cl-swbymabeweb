(ql:quickload :cl-swbymabeweb)

(defpackage cl-swbymabeweb.app
  (:use :cl :log4cl)
  (:import-from #:cl-swbymabeweb.config
                #:*logs-directory*))
(in-package :cl-swbymabeweb.app)

(log:config :info :sane :daily "logs/app.log" :backup nil)
