(ql:quickload :cl-swbymabeweb)

(defpackage cl-swbymabeweb.app
  (:use :cl)
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

(builder
 (:static
  :path (lambda (path)
          (if (ppcre:scan "^(?:/gfx/|/css/|/js/|/download/|/robot\\.txt$|/favicon\\.ico$)" path)
              path
              nil))
  :root *static-directory*)
 :accesslog
 (if (getf (config) :error-log)
     `(:backtrace
       :output ,(getf (config) :error-log))
     nil)
 :session
 ;; (if (productionp)
 ;;     nil
 ;;     (lambda (app)
 ;;       (lambda (env)
 ;;         (let ((datafly:*trace-sql* t))
 ;;           (funcall app env)))))
 *web*)
