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
    (let ((all-blogs (cdr (repo-get-all))))
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
        (is (string= "<h2>Heading 1</h2>

<p><a href='/foo' class='link'>MyLink</a></p>

<p><code>foo</code></p>

<pre><code>tripple
quote
block</code></pre>
"
                     (blog-entry-text blog)))))))

(test get-latest
  "Tests getting the latest file."
  (with-fixture fixture ()
    (let ((latest (cdr (repo-get-latest))))
      (is (string= "test 2" (blog-entry-name latest))))))

(test get-for-name--ok
  "Get blog entry for name."
  (with-fixture fixture ()
    (let ((blog (cdr (repo-get-for-name "test 1"))))
      (is (string= "test 1" (blog-entry-name blog))))))

(test get-for-name--nok
  "Get blog entry for name."
  (with-fixture fixture ()
    (let ((blog (repo-get-for-name "not-exists")))
      (is (eq :not-found-error (car blog))))))

(defun run-tests ()
  (run! 'create-blog-repo)
  (run! 'get-all)
  (run! 'get-latest)
  (run! 'get-for-name--ok)
  (run! 'get-for-name--nok))


;; TODO: add output interface to as (cons x y)
