(in-package :cl-user)
(defpackage :cl-swbymabeweb-test
  (:use :cl :fiveam :str)
  (:local-nicknames (:dex :dexador))
  (:import-from #:cl-swbymabeweb
                #:start
                #:stop))
(in-package :cl-swbymabeweb-test)

(def-suite it-routing)
(in-suite it-routing)

(def-fixture with-server ()
  (start)
  (sleep 0.5)
  (&body)
  (stop)
  (sleep 0.5))

(test handle-index-route
  "Test routing of index."
  (with-fixture with-server ()
    (is (str:containsp "<title>Manfred Bergmann | Software Development | Index</title>"
                       (dex:get "http://localhost:5000/")))))

(test handle-undefined-route
  "Test route that doesn't exist."
  (with-fixture with-server ()
    (handler-case
        (progn
          (dex:get "http://localhost:5000/doesnotexist")
          (is nil))
      (dex:http-request-not-found (e)
        (is (= (dex:response-status e) 404))))))

(run! 'handle-index-route)
(run! 'handle-undefined-route)
