(defpackage :cl-swbymabeweb.blog-repo-test
  (:use :cl :fiveam :cl-swbymabeweb.blog-repo)
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
  (let* ((folder (merge-pathnames "test-blogs/"
                                  (asdf/system:system-source-directory
                                   (asdf:find-system "cl-swbymabeweb"))))
         (cut (make-instance 'blog-repo-default
                             :blog-folder folder)))
    (format t "using blogs folder: ~a~%" (uiop:native-namestring folder))
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
      ;; file one - sorted by date
      (let ((blog (first all-blogs)))
        (is (string= "test 2" (blog-entry-name blog)))
        (is (= (parse-date-time "20200611") (blog-entry-date blog)))
        (is (string= (format nil "<div>~%  <h2>Heading 2</h2>~%  <span>Foo</span>~%</div>~%")
                     (blog-entry-text blog))))
      ;; file two
      (let ((blog (second all-blogs)))
        (is (string= "test 1" (blog-entry-name blog)))
        (is (= (parse-date-time "20190412") (blog-entry-date blog)))
        (is (string= (format nil "## Heading 1~%~%`foo`~%")
                     (blog-entry-text blog)))))))

(test get-latest
  "Tests getting the latest file."
  )


(defun test-all ()
  (run! 'create-blog-repo)
  (run! 'get-all)
  (run! 'get-latest))
