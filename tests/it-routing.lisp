(in-package :cl-user)
(defpackage :cl-swbymabeweb-test
  (:use :cl :fiveam :cl-mock :str)
  (:local-nicknames (:dex :dexador))
  (:import-from #:cl-swbymabeweb
                #:start
                #:stop))
(in-package :cl-swbymabeweb-test)

(def-suite it-routing
  :description "Routing integration tests."
  :in cl-swbymabeweb.tests:test-suite)

(in-suite it-routing)

(def-fixture with-server ()
  (start)
  (sleep 0.5)
  (unwind-protect 
       (&body)
    (stop))
  (stop)
  (sleep 0.5))

(test handle-index-route
  "Test routing of index."
  (with-fixture with-server ()
    (is (str:containsp "<title>Manfred Bergmann | Software Development | Index</title>"
                       (dex:get "http://localhost:5000/")))))

(test handle-imprint-route
  "Test routing of imprint."
  (with-fixture with-server ()
    (is (str:containsp "<title>Manfred Bergmann | Software Development | Imprint</title>"
                       (dex:get "http://localhost:5000/imprint")))))

(test handle-about-route
  "Test routing of about."
  (with-fixture with-server ()
    (is (str:containsp "<title>Manfred Bergmann | Software Development | About</title>"
                       (dex:get "http://localhost:5000/about")))))

(test handle-blog-index-route
  "Test routing of blog - index."
  (with-fixture with-server ()
    (is (str:containsp "<title>Manfred Bergmann | Software Development | Blog</title>"
                         (dex:get "http://localhost:5000/blog")))))

(test handle-blog-route-with-blog-name
  "Test routing of blog with name of blog."
  (with-fixture with-server ()
    (is (str:containsp "<title>Manfred Bergmann | Software Development | Blog</title>"
                       (dex:get "http://localhost:5000/blog/my+first+blog")))))

(test handle-blog-route-with-blog-name-not-found
  "Test routing of blog with name of blog."
  (with-fixture with-server ()
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

(run! 'handle-index-route)
(run! 'handle-imprint-route)
(run! 'handle-about-route)

(run! 'handle-blog-index-route)
(run! 'handle-blog-route-with-blog-name)
(run! 'handle-blog-route-with-blog-name-not-found)
(run! 'handle-undefined-route)
