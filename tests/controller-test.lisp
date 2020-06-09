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

    (is (string= (cdr (controller.index:index)) (fake-index-page)))
    (is (= 1 (length (invocations 'view.index:render))))))

(test imprint-controller
  "Test imprint controller"

  (with-mocks ()
    (answer (view.imprint:render) (fake-imprint-page))

    (is (string= (cdr (controller.imprint:index)) (fake-imprint-page)))
    (is (= 1 (length (invocations 'view.imprint:render))))))

(test about-controller
  "Test about controller"

  (with-mocks ()
    (answer (view.about:render) (fake-about-page))

    (is (string= (cdr (controller.about:index)) (fake-about-page)))
    (is (= 1 (length (invocations 'view.about:render))))))

(defparameter *blog-entry* nil)
(defparameter *blog-model* nil)

(test blog-controller-index
  "Test blog controller for index which shows the latest blog entry"

  (setf *blog-entry*
        (blog-repo:make-blog-entry "Foobar"
                              (now)
                              "<b>hello world</b>"))
  (setf *blog-model*
        (make-instance 'blog-view-model
                       :blog-post (blog-entry-to-blog-post *blog-entry*)))
  (with-mocks ()
    (answer (blog-repo:repo-get-latest) (cons :ok *blog-entry*))
    (answer (view.blog:render *blog-model*) (fake-blog-page))

    (is (string= (cdr (controller.blog:index)) (fake-blog-page)))
    (is (= 1 (length (invocations 'view.blog:render))))
    (is (= 1 (length (invocations 'blog-repo:repo-get-latest))))))

(test blog-controller-index-no-blog-entry
  "Test blog controller when there is no blog entry available"

  (setf *blog-model*
        (make-instance 'blog-view-model
                       :blog-post nil))
  (with-mocks ()
    (answer (blog-repo:repo-get-latest) (cons :ok nil))
    (answer (view.blog:render *blog-model*) (fake-blog-page))

    (is (string= (cdr (controller.blog:index)) (fake-blog-page)))
    (is (= 1 (length (invocations 'view.blog:render))))
    (is (= 1 (length (invocations 'blog-repo:repo-get-latest))))))

(test blog-controller-for-blog-name
  "Test blog controller to render names blog entry"

  (setf *blog-entry*
        (blog-repo:make-blog-entry "my_blog_name"
                              (now)
                              "<b>hello world</b>"))
  (setf *blog-model*
        (make-instance 'blog-view-model
                       :blog-post (blog-entry-to-blog-post *blog-entry*)))
  (with-mocks ()
    (answer (blog-repo:repo-get-blog-entry name) (if (string= name "my blog name")
                                           (cons :ok *blog-entry*)
                                           (error "wrong provided blog name!")))
    (answer (view.blog:render *blog-model*) (fake-blog-page))

    (is (string= (cdr (controller.blog:for-blog-name "my blog name")) (fake-blog-page)))
    (is (= 1 (length (invocations 'view.blog:render))))
    (is (= 1 (length (invocations 'blog-repo:repo-get-blog-entry))))))

(test blog-controller-for-blog-name-not-found
  "Test blog controller with blog name that doesn't exist."

  (with-mocks ()
    (answer (blog-repo:repo-get-blog-entry _)
      (cons :not-found-error "Post 'my blog post' doesn't exist!"))

    (let ((controller-result (controller.blog:for-blog-name "my blog name")))
      (is (equalp (cons :not-found-error "Post 'my blog post' doesn't exist!")
                  controller-result)))
    (is (= 0 (length (invocations 'view.blog:render))))
    (is (= 1 (length (invocations 'blog-repo:repo-get-blog-entry))))))

(run! 'index-controller)
(run! 'imprint-controller)
(run! 'about-controller)
(run! 'blog-controller-index)
(run! 'blog-controller-index-no-blog-entry)
(run! 'blog-controller-for-blog-name)
(run! 'blog-controller-for-blog-name-not-found)
