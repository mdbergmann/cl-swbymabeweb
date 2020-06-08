(defpackage :cl-swbymabeweb.view-test
  (:use :cl :fiveam :str :blog :view.blog)
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
(defparameter *expected-imprint-page-title*
  "Manfred Bergmann | Software Development | Imprint")
(defparameter *expected-about-page-title*
  "Manfred Bergmann | Software Development | About")
(defparameter *expected-blog-page-title*
  "Manfred Bergmann | Software Development | Blog")


(test index-view
  "Index view renders empty page with only navigation but no content."
  (let ((page-source (view.index:render)))
    (format t "~a~%" page-source)
    (is (str:containsp *expected-index-page-title* page-source))
    (is (str:containsp "<div id=navigation" page-source))))

(test imprint-view
  "Imprint view"
  (let ((page-source (view.imprint:render)))
    (format t "~a~%" page-source)
    (is (str:containsp *expected-imprint-page-title* page-source))
    (is (str:containsp "<div id=navigation" page-source))
    (is (str:containsp "<div id=content" page-source))))

(test about-view
  "About view"
  (let ((page-source (view.about:render)))
    (format t "~a~%" page-source)
    (is (str:containsp *expected-about-page-title* page-source))
    (is (str:containsp "<div id=navigation" page-source))
    (is (str:containsp "<div id=content" page-source))))

(defparameter *blog-view-model*
  (make-instance 'blog-view-model
                 :blog-post (make-instance 'blog-post
                                           :name "Foo"
                                           :date "22.9.1973"
                                           :text "Foobar")))
(defparameter *blog-view-empty-model*
  (make-instance 'blog-view-model
                 :blog-post nil))

(test blog-view
  "Blog view renders latest blog entry, if exists."
  (let* ((page-source (view.blog:render *blog-view-model*)))
    (format t "~a~%" page-source)
    (is (str:containsp *expected-blog-page-title* page-source))
    (is (str:containsp "<div id=navigation" page-source))
    (is (str:containsp "<div id=content" page-source))
    (is (str:containsp "Foo" page-source))
    (is (str:containsp "Foobar" page-source))
    (is (str:containsp "22.9.1973" page-source))))

(test blog-view-nil-model-post
  "Test blog view to show empty div when there is no blog post to show."
  (let* ((page-source (view.blog:render *blog-view-empty-model*)))
    (format t "~a~%" page-source)
    (is (str:containsp *expected-blog-page-title* page-source))
    (is (str:containsp "<div id=navigation" page-source))
    (is (str:containsp "<tr><td class=content colspan=2><tr>" page-source))))

(run! 'index-view)
(run! 'imprint-view)
(run! 'about-view)
(run! 'blog-view)
(run! 'blog-view-nil-model-post)
