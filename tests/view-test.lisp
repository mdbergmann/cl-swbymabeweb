(defpackage :cl-swbymabeweb.view-test
  (:use :cl :fiveam)
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

(test index-view
  "Index view renders empty page with only navigation but no content."
  (let* ((spinneret:*html-style* :tree)
         (page-source (view.index:render)))
    (format t "~a~%" page-source)
    (is (str:containsp *expected-index-page-title* (view.index:render)))
    (is (str:containsp "<div id=navigation" (view.index:render)))))

(test imprint-view
  "Imprint view renders empty page."
  (let* ((spinneret:*html-style* :tree)
         (page-source (view.imprint:render)))
    (format t "~a~%" page-source)
    (is (str:containsp *expected-imprint-page-title* (view.imprint:render)))
    (is (str:containsp "<div id=navigation" (view.imprint:render)))
    (is (str:containsp "<div id=content" (view.imprint:render)))))

(test about-view
  "About view renders empty page."
  (let* ((spinneret:*html-style* :tree)
         (page-source (view.about:render)))
    (format t "~a~%" page-source)
    (is (str:containsp *expected-about-page-title* (view.about:render)))
    (is (str:containsp "<div id=navigation" (view.about:render)))
    (is (str:containsp "<div id=content" (view.about:render)))))

(run! 'index-view)
(run! 'imprint-view)
(run! 'about-view)
