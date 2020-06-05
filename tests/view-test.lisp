(defpackage :cl-swbymabeweb.view-test
  (:use :cl :fiveam :cl-swbymabeweb.view.index)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-swbymabeweb.view-test)

(def-suite view-tests
  :description "View tests"
  :in cl-swbymabeweb.tests:test-suite)

(in-suite view-tests)

(defparameter *expected-index-page-title*
  "Manfred Bergmann | Software Development | Index")

(test index-view
  "Index view renders empty page with only navigation but no content."
  (let* ((spinneret:*html-style* :tree)
         (page-source (view.index:render)))
    (format t "~a~%" page-source)
    (is (str:containsp *expected-index-page-title* (view.index:render)))
    (is (str:containsp "<div id=navigation" (view.index:render)))))

(run! 'index-view)
