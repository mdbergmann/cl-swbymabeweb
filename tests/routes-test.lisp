(defpackage :cl-swbymabeweb.routes-test
  (:use :cl :fiveam :cl-mock :cl-swbymabeweb.routes)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-swbymabeweb.routes-test)

(def-suite routes-tests
    :description "Routes unit tests"
    :in cl-swbymabeweb.tests:test-suite)

(in-suite routes-tests)

(defmacro with-request ((uri
                         &rest morekeys
                         &key &allow-other-keys) args
                        &body body)
  (let ((result-sym (gensym)))
    `(let* ((snooze:*catch-errors* nil)
            (snooze:*catch-http-conditions* t)
            (,result-sym
              (multiple-value-list
               (snooze:handle-request
                ,uri
                ,@morekeys)))
            ,@(loop for arg in args
                    for i from 0
                    when arg
                      collect `(,arg (nth ,i ,result-sym))))
       ,@body)))

(test home-route
  "Tests the home/index route"

  (with-mocks ()
    (answer (controller.index:index) (cons :ok ""))

    (with-request ("/") (code)
      (is (= 200 code))
      (is (= 1 (length (invocations 'controller.index:index)))))))

(test imprint-route
  "Tests the imprint index route"

  (with-mocks ()
    (answer (controller.imprint:index) (cons :ok ""))

    (with-request ("/imprint") (code)
      (is (= 200 code))
      (is (= 1 (length (invocations 'controller.imprint:index)))))))

(test about-route
  "Tests the about index route"

  (with-mocks ()
    (answer (controller.about:index) (cons :ok ""))

    (with-request ("/about") (code)
      (is (= 200 code))
      (is (= 1 (length (invocations 'controller.about:index)))))))

(test projects-route
  "Tests the projects index route"
  (with-mocks ()
    (answer (controller.projects:index) (cons :ok ""))

    (with-request ("/projects") (code)
      (is (= 200 code))
      (is (= 1 (length (invocations 'controller.projects:index)))))))

(test blog-route-index
  "Tests the blog index route. No name"

  (with-mocks ()
    (answer (controller.blog:index) (cons :ok ""))

    (with-request ("/blog") (code)
      (is (= 200 code))
      (is (= 1 (length (invocations 'controller.blog:index)))))))

(test blog-route-index-err
  "Tests the blog index route. No name. 400 error"

  (with-mocks ()
    (answer (controller.blog:index) (cons :error "Foo"))

    (with-request ("/blog") (code)
      (is (= 500 code))
      (is (= 1 (length (invocations 'controller.blog:index)))))))

(test blog-route-for-name
  "Tests the blog route with blog name"

  (with-mocks ()
    (answer (controller.blog:for-blog-name blog-name)
      (progn
        (assert (string= blog-name "FOO BAR"))
        (cons :ok "")))

    (with-request ("/blog/foo+bar") (code)
      (is (= 200 code))
      (is (= 1 (length (invocations 'controller.blog:for-blog-name)))))))

(test blog-route-for-name--not-exists
  "Tests the blog route with blog name that doesn't exist"

  (with-mocks ()
    (answer (controller.blog:for-blog-name _) (cons :not-found-error ""))

    (with-request ("/blog/foo+bar") (code)
      (is (= 404 code))
      (is (= 1 (length (invocations 'controller.blog:for-blog-name)))))))

(test blog-route-for-name--other-err
  "Tests the blog route with blog name. 400 code."

  (with-mocks ()
    (answer (controller.blog:for-blog-name _)
      (cons :error "some error"))

    (with-request ("/blog/foo+bar") (code)
      (is (= 400 code))
      (is (= 1 (length (invocations 'controller.blog:for-blog-name)))))))

(test blog-route-atom-feed
  "Tests the blog route for the atom feed"

  (with-mocks ()
    (answer (controller.blog:atom-feed) (cons :ok "<some>xml</some>"))

    (with-request ("/blog-atom-feed") (code)
      (is (= 200 code))
      (is (= 1 (length (invocations 'controller.blog:atom-feed)))))))

(test blog-route-atom-feed--err
  "Tests the blog route for the atom feed. 400 code."

  (with-mocks ()
    (answer (controller.blog:atom-feed) (cons :error "some error"))

    (with-request ("/blog-atom-feed") (code)
      (is (= 400 code))
      (is (= 1 (length (invocations 'controller.blog:atom-feed)))))))
