(defpackage :cl-swbymabeweb.controller-test
  (:use :cl :fiveam :cl-mock)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-swbymabeweb.controller-test)

(def-suite controller-tests
  :description "Tests for page controllers"
  :in cl-swbymabeweb.tests:test-suite)

(in-suite controller-tests)

;; your test code here

(defparameter *expected-page-title-index* "Manfred Bergmann | Software Development | Index")
(defparameter *expected-page-title-imprint* "Manfred Bergmann | Software Development | Index")
(defparameter *expected-page-title-about* "Manfred Bergmann | Software Development | About")

(defun fake-index-page ()
  *expected-page-title-index*)

(defun fake-imprint-page ()
  *expected-page-title-imprint*)

(defun fake-about-page ()
  *expected-page-title-about*)

(test index-controller
  "Test index controller"

  (with-mocks ()
    ;; we want to call 'render' on an index page view
    (answer (view.index:render) (fake-index-page))

    (is (string= (controller.index:index) (fake-index-page)))
    (is (= (length (invocations 'view.index:render)) 1))))

(test imprint-controller
  "Test imprint controller"

  (with-mocks ()
    (answer (view.imprint:render) (fake-imprint-page))

    (is (string= (controller.imprint:index) (fake-imprint-page)))
    (is (= (length (invocations 'view.imprint:render)) 1))))

(test about-controller
  "Test about controller"

  (with-mocks ()
    (answer (view.about:render) (fake-about-page))

    (is (string= (controller.about:index) (fake-about-page)))
    (is (= (length (invocations 'view.about:render)) 1))))

(run! 'index-controller)
(run! 'imprint-controller)
(run! 'about-controller)
