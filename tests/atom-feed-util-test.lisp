(defpackage :cl-swbymabeweb.atom-feed-test
  (:use :cl :fiveam :cl-swbymabeweb.atom-feed)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-swbymabeweb.atom-feed-test)

(def-suite atom-feed-tests
  :description "Tests the atom feed generation."
  :in cl-swbymabeweb.tests:test-suite)

(in-suite atom-feed-tests)

(test atom-feed-generate
  "Test the atom feed generation."

  
  )
