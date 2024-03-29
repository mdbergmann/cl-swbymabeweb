(defpackage :cl-swbymabeweb.view-test
  (:use :cl :fiveam :str :blog-repo :view.blog)
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
(defparameter *expected-projects-page-title*
  "Manfred Bergmann | Software Development | Projects")
(defparameter *expected-blog-page-title*
  "Manfred Bergmann | Software Development | Blog")


(test index-view
  "Index view renders empty page with only navigation but no content."
  (let ((page-source (view.index:render)))
    (print page-source)
    (is (str:containsp *expected-index-page-title* page-source))
    (is (str:containsp "<div class='header_logo'" page-source))
    (is (str:containsp "<div class='header_nav'" page-source))
    (is (str:containsp "<div class='content'" page-source))
    (is (str:containsp "<div class='sub_content'" page-source))
    (is (str:containsp "<p style='font-size: 36pt;'>Hello!" page-source))))

(test imprint-view
  "Imprint view"
  (let ((page-source (view.imprint:render (lambda () "<p>Hello Foo</p>"))))
    (is (str:containsp *expected-imprint-page-title* page-source))
    (is (str:containsp "<div class='header_logo'" page-source))
    (is (str:containsp "<div class='header_nav'" page-source))
    (is (str:containsp "<div class='content'" page-source))
    (is (str:containsp "<div class='sub_content'" page-source))
    (is (str:containsp "<p>Hello Foo</p>" page-source))))

(test about-view
  "About view"
  (let ((page-source (view.about:render (lambda () "<p>Hello Bar</p>"))))
    (is (str:containsp *expected-about-page-title* page-source))
    (is (str:containsp "<div class='header_logo'" page-source))
    (is (str:containsp "<div class='header_nav'" page-source))
    (is (str:containsp "<div class='content'" page-source))
    (is (str:containsp "<div class='sub_content'" page-source))
    (is (str:containsp "<p>Hello Bar</p>" page-source))))

(test projects-view
  "Projects view"
  (let ((page-source (view.projects:render (lambda () "<p>Hello Buzz</p>"))))
    (is (str:containsp *expected-projects-page-title* page-source))
    (is (str:containsp "<div class='header_logo'" page-source))
    (is (str:containsp "<div class='header_nav'" page-source))
    (is (str:containsp "<div class='content'" page-source))
    (is (str:containsp "<div class='sub_content'" page-source))
    (is (str:containsp "<p>Hello Buzz</p>" page-source))))


(defparameter *blog-post* (make-instance 'blog-post-model
                                         :name "Foo"
                                         :date "22 September 2020"
                                         :nav-date "22-09-2020"
                                         :text "Foobar"))
(defparameter *blog-view-model*
  (make-instance 'blog-view-model
                 :blog-post *blog-post*
                 :all-blog-posts (list *blog-post*)
                 :atom-url "http://foo.bar/atom"))
(defparameter *blog-view-empty-model*
  (make-instance 'blog-view-model
                 :blog-post nil
                 :all-blog-posts (list *blog-post*)
                 :atom-url "http://foo.bar/atom"))

(test blog-view
  "Blog view renders latest blog entry, if exists."
  (let ((page-source (view.blog:render *blog-view-model*)))
    (print page-source)
    (is (str:containsp *expected-blog-page-title* page-source))
    (is (str:containsp "<div class='article'" page-source))
    (is (str:containsp "<div class='recent_articles'" page-source))

    (is (str:containsp "Foo" page-source))
    (is (str:containsp "Foobar" page-source))
    (is (str:containsp "22-09-2020" page-source))
    (is (str:containsp "22 September 2020" page-source))

    (is (str:containsp "<a href='http://foo.bar/atom'>[atom/rss feed]" page-source))
    ))

(test blog-view-nil-model-post
  "Test blog view to show empty div when there is no blog post to show."
  (let ((page-source (view.blog:render *blog-view-empty-model*)))
    (is (str:containsp *expected-blog-page-title* page-source))
    (is (str:containsp "<div class='recent_articles'" page-source))
    ))
