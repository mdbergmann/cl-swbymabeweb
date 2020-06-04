(in-package :cl-user)
(defpackage :cl-swbymabeweb.controller-test
  (:use :cl :fiveam :cl-mock :cl-swbymabeweb.controller.index)
  (:export #:run!
           #:all-tests
           #:nil)
  (:import-from #:cl-swbymabeweb.view.index
                #:render)
  (:import-from #:cl-swbymabeweb.view.common
                #:with-page))
(in-package :cl-swbymabeweb.controller-test)

(def-suite controller-tests
  :description "Tests for page controllers"
  ;;:in cl-swbymabeweb.tests:test-suite
  )

(in-suite controller-tests)

;; your test code here

(defparameter *index-page-title* "Manfred Bergmann | Software Development | Index")

(defun fake-index-page ()
  (view.common:with-page (:title *index-page-title*)))

(test index-controller
  "Test index controller"

  (with-mocks ()
    ;; we want to call 'render' on an index page view
    (answer (view.index:render) (fake-index-page))

    (is (string= (controller.index:index) (fake-index-page)))
    (is (= (length (invocations 'view.index:render)) 1))))

(run! 'index-controller)
