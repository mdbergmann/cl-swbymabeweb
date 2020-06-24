(defpackage :cl-swbymabeweb-test
  (:use :cl :fiveam :local-time :str)
  (:local-nicknames (:dex :dexador))
  (:import-from #:cl-swbymabeweb
                #:start
                #:stop))
(in-package :cl-swbymabeweb-test)

(def-suite it-routing
  :description "Routing integration tests."
  :in cl-swbymabeweb.tests:test-suite)

(in-suite it-routing)

(defclass blog-repo-fake (blog-repo:blog-repo-base) ())
(defclass blog-repo-fake-not-found-for-name (blog-repo-fake) ())

(defmethod blog-repo::get-latest ((self blog-repo-fake))
  nil)
(defmethod blog-repo::get-all ((self blog-repo-fake))
  (list (blog-repo:make-blog-entry "Foo" (get-universal-time) "Some text")))
(defmethod blog-repo::get-for-name ((self blog-repo-fake) name)
  (declare (ignore name))
  (blog-repo:make-blog-entry "Foo" (get-universal-time) "Some text"))
(defmethod blog-repo::get-for-name ((self blog-repo-fake-not-found-for-name) name)
  (declare (ignore name))
  nil)

(def-fixture with-server ()
  (start)
  (blog-repo:blog-repo-fac-init (make-instance 'blog-repo-fake))
  (sleep 0.5)
  (unwind-protect 
       (&body)
    (stop)
    (sleep 0.5)
    (blog-repo:blog-repo-fac-clean)))

(test handle-index-route
  "Test routing of index."
  (with-fixture with-server ()
    (is (str:containsp "<title>Manfred Bergmann | Software Development | Index"
                       (dex:get "http://localhost:5000/")))))

(test handle-imprint-route
  "Test routing of imprint."
  (with-fixture with-server ()
    (is (str:containsp "<title>Manfred Bergmann | Software Development | Imprint"
                       (dex:get "http://localhost:5000/imprint")))))

(test handle-about-route
  "Test routing of about."
  (with-fixture with-server ()
    (is (str:containsp "<title>Manfred Bergmann | Software Development | About"
                       (dex:get "http://localhost:5000/about")))))

(test handle-blog-index-route
  "Test routing of blog - index."
  (with-fixture with-server ()
    (is (str:containsp "<title>Manfred Bergmann | Software Development | Blog"
                         (dex:get "http://localhost:5000/blog")))))

(test handle-blog-route-with-blog-name
  "Test routing of blog with name of blog."
  (with-fixture with-server ()
    (is (str:containsp "<title>Manfred Bergmann | Software Development | Blog"
                       (dex:get "http://localhost:5000/blog/my+first+blog")))))

(test handle-blog-route-with-blog-name-not-found
  "Test routing of blog with name of blog."
  (with-fixture with-server ()

    (blog-repo:blog-repo-fac-init (make-instance 'blog-repo-fake-not-found-for-name))
    
    (handler-case
        (progn
          (dex:get "http://localhost:5000/blog/name+not+found")
          (is-true nil))
      (dex:http-request-not-found (e)
        (is (= (dex:response-status e) 404))))))

(test handle-undefined-route
  "Test route that doesn't exist."
  (with-fixture with-server ()
    (handler-case
        (progn
          (dex:get "http://localhost:5000/doesnotexist")
          (is-true nil))
      (dex:http-request-not-found (e)
        (is (= (dex:response-status e) 404))))))

(test handle-blog-atom
  "Tests the route for blog atom."

  (with-fixture with-server ()
    (let ((atom-result (dex:get "http://localhost:5000/blog/atom.xml")))
      (is (str:containsp "feed xmlns=\"http://www.w3.org/2005/Atom\"" atom-result)))))

(defun run-tests ()
  (run! 'handle-index-route)
  (run! 'handle-imprint-route)
  (run! 'handle-about-route)

  (run! 'handle-blog-index-route)
  (run! 'handle-blog-route-with-blog-name)
  (run! 'handle-blog-route-with-blog-name-not-found)
  (run! 'handle-undefined-route)
  (run! 'handle-blog-atom))
