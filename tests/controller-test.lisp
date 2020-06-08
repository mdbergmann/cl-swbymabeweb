(defpackage :cl-swbymabeweb.controller-test
  (:use :cl :fiveam :cl-mock :local-time :view.blog :blog)
  (:export #:run!
           #:all-tests
           #:nil)
  (:import-from #:controller.blog
                #:blog-entry-to-blog-post))
(in-package :cl-swbymabeweb.controller-test)

(def-suite controller-tests
  :description "Tests for page controllers"
  :in cl-swbymabeweb.tests:test-suite)

(in-suite controller-tests)

;; your test code here

(defparameter *expected-page-title-index*
  "Manfred Bergmann | Software Development | Index")
(defparameter *expected-page-title-imprint*
  "Manfred Bergmann | Software Development | Imprint")
(defparameter *expected-page-title-about*
  "Manfred Bergmann | Software Development | About")
(defparameter *expected-page-title-blog*
  "Manfred Bergmann | Software Development | Blog")

(defun fake-index-page ()
  *expected-page-title-index*)

(defun fake-imprint-page ()
  *expected-page-title-imprint*)

(defun fake-about-page ()
  *expected-page-title-about*)

(defun fake-blog-page ()
  *expected-page-title-blog*)

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

(defparameter *latest-blog-entry*
  (blog:make-blog-entry "Foobar"
                        (now)
                        "<b>hello world</b>"))
(defparameter *blog-model*
  (make-instance 'blog-view-model
                 :blog-post (blog-entry-to-blog-post *latest-blog-entry*)))

(test blog-controller-index
  "Test blog controller for index which shows the latest blog entry"

  (with-mocks ()
    (answer (blog:get-latest) *latest-blog-entry*)
    (answer (view.blog:render *blog-model*) (fake-blog-page))

    (is (string= (controller.blog:index) (fake-blog-page)))
    (print (invocations 'view.blog:render))
    (print (cadr (car (invocations 'view.blog:render))))
    (is (= (length (invocations 'view.blog:render)) 1))
    (is (= (length (invocations 'blog:get-latest)) 1))))

(test blog-controller-index-no-blog-entry
  "Test blog controller when there is no blog entry available."

  (with-mocks ()
    (answer (blog:get-latest) nil)
    (answer (view.blog:render *blog-model*) (fake-blog-page))

    (is (string= (controller.blog:index) (fake-blog-page)))
    (is (= (length (invocations 'view.blog:render)) 1))
    (is (= (length (invocations 'blog:get-latest)) 1))))

(run! 'index-controller)
(run! 'imprint-controller)
(run! 'about-controller)
(run! 'blog-controller-index)
(run! 'blog-controller-index-no-blog-entry)
