(defpackage :cl-swbymabeweb.blog-repo-test
  (:use :cl :fiveam :local-time :cl-swbymabeweb.blog-repo)
  (:export #:run!
           #:all-tests
           #:nil)
  (:import-from #:cl-date-time-parser
                #:parse-date-time))
(in-package :cl-swbymabeweb.blog-repo-test)

(def-suite blog-repo-tests
  :description "Tests for the main blog-repo."
  :in cl-swbymabeweb.tests:test-suite)

(in-suite blog-repo-tests)

(def-fixture fixture ()
  (let ((cut (make-instance 'blog-repo-default
                            :blog-folder #P"../test-blogs/")))
    (blog-repo-fac-init cut)
    (&body)))

(test create-blog-repo
  "Creating a blog-repo."
  (is (typep (make-instance 'blog-repo-default :blog-folder #P"") 'blog-repo-default)))

(test get-all
  "Tests get all blog entries."
  (with-fixture fixture ()
    (let ((all-blogs (repo-get-all)))
      (format t "blogs: ~a~%" all-blogs)
      (is (= 2 (length all-blogs)))
      (is (every #'blog-entry-p all-blogs))
      ;; file one
      (let ((first (first all-blogs)))
        (is (string= "test 1" (blog-entry-name first)))
        (is (timestamp= (universal-to-timestamp (parse-date-time "20190412"))
                        (blog-entry-date first)))
        (is (string= (format nil "## Heading 1~%~%`foo`~%")
                     (blog-entry-text first))))
      ;; file two
      (let ((second (second all-blogs)))
        (is (string= "test 2" (blog-entry-name second)))
        (is (timestamp= (universal-to-timestamp (parse-date-time "20200611"))
                        (blog-entry-date second)))
        (is (string= (format nil "<div>~%  <h2>Heading 2</h2>~%  <span>Foo</span>~%</div>~%")
                     (blog-entry-text second)))))))
