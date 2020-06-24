(defpackage :cl-swbymabeweb.controller-test
  (:use :cl :fiveam :cl-mock :local-time :view.blog :blog-repo)
  (:export #:run!
           #:all-tests
           #:nil)
  (:import-from #:controller.blog
                #:blog-entry-to-blog-post)
  (:import-from #:cl-swbymabeweb.config
                #:*application-root*))
(in-package :cl-swbymabeweb.controller-test)

(def-suite controller-tests
  :description "Tests for page controllers"
  :in cl-swbymabeweb.tests:test-suite)

(in-suite controller-tests)

;; your test code here

(defparameter *expected-page-title-index*
  "Manfred Bergmann | Software Development | Index")
(defparameter *expected-page-title-imprint*
  "Manfred Bergmann | Software Development | Imprint")
(defparameter *expected-page-title-about*
  "Manfred Bergmann | Software Development | About")
(defparameter *expected-page-title-blog*
  "Manfred Bergmann | Software Development | Blog")

(defun fake-index-page ()
  *expected-page-title-index*)

(defun fake-blog-page ()
  *expected-page-title-blog*)


(test index-controller
  "Test index controller"

  (with-mocks ()
    (answer (view.index:render) (fake-index-page))

    (is (string= (cdr (controller.index:index)) (fake-index-page)))
    (is (= 1 (length (invocations 'view.index:render))))))

(test imprint-controller
  "Test imprint controller"

  (with-mocks ()
    (answer (view.imprint:render content-fun) (funcall content-fun))

    (let ((imprint-result (controller.imprint:index)))
      (print imprint-result)
      (is (eq :ok (car imprint-result)))
      (is (str:starts-with-p "<p>Manfred Bergmann<br/>"
                             (cdr imprint-result))))
    (is (= 1 (length (invocations 'view.imprint:render))))))

(test about-controller
  "Test about controller"

  (with-mocks ()
    (answer (view.about:render content-fun) (funcall content-fun))

    (let ((imprint-result (controller.about:index)))
      (print imprint-result)
      (is (eq :ok (car imprint-result)))
      (is (str:starts-with-p "<p>Passionate about software development"
                             (cdr imprint-result))))
    (is (= 1 (length (invocations 'view.about:render))))))


;; -----------------------------------
;; blog controller -------------------
;; -----------------------------------

(defparameter *blog-entry* nil)
(defparameter *blog-model* nil)

(test blog-controller-index
  "Test blog controller for index which shows the latest blog entry"

  (setf *blog-entry*
        (blog-repo:make-blog-entry "Foobar"
                                   (get-universal-time)
                                   "<b>hello world</b>"))
  (setf *blog-model*
        (make-instance 'blog-view-model
                       :blog-post (blog-entry-to-blog-post *blog-entry*)
                       :all-blog-posts (mapcar #'blog-entry-to-blog-post (list *blog-entry*))))
  (with-mocks ()
    (answer (blog-repo:repo-get-latest) (cons :ok *blog-entry*))
    (answer (blog-repo:repo-get-all) (cons :ok (list *blog-entry*)))
    (answer (view.blog:render *blog-model*) (fake-blog-page))

    (is (string= (cdr (controller.blog:index)) (fake-blog-page)))
    (is (= 1 (length (invocations 'view.blog:render))))
    (is (= 1 (length (invocations 'blog-repo:repo-get-all))))
    (is (= 1 (length (invocations 'blog-repo:repo-get-latest))))))

(test blog-controller-index-no-blog-entry
  "Test blog controller when there is no blog entry available"

  (setf *blog-model*
        (make-instance 'blog-view-model
                       :blog-post nil))
  (with-mocks ()
    (answer (blog-repo:repo-get-latest) (cons :ok nil))
    (answer (blog-repo:repo-get-all) (cons :ok (list *blog-entry*)))
    (answer (view.blog:render *blog-model*) (fake-blog-page))

    (is (string= (cdr (controller.blog:index)) (fake-blog-page)))
    (is (= 1 (length (invocations 'view.blog:render))))
    (is (= 1 (length (invocations 'blog-repo:repo-get-all))))
    (is (= 1 (length (invocations 'blog-repo:repo-get-latest))))))

(test blog-controller-for-blog-name
  "Test blog controller to render names blog entry"

  (setf *blog-entry*
        (blog-repo:make-blog-entry "my_blog_name"
                                   (get-universal-time)
                                   "<b>hello world</b>"))
  (setf *blog-model*
        (make-instance 'blog-view-model
                       :blog-post (blog-entry-to-blog-post *blog-entry*)))
  (with-mocks ()
    (answer (blog-repo:repo-get-for-name name)
      (if (string= name "my blog name")
          (cons :ok *blog-entry*)
          (error "wrong provided blog name!")))
    (answer (blog-repo:repo-get-all) (cons :ok (list *blog-entry*)))
    (answer (view.blog:render *blog-model*) (fake-blog-page))

    (is (string= (cdr (controller.blog:for-blog-name "my blog name")) (fake-blog-page)))
    (is (= 1 (length (invocations 'view.blog:render))))
    (is (= 1 (length (invocations 'blog-repo:repo-get-all))))
    (is (= 1 (length (invocations 'blog-repo:repo-get-for-name))))))

(test blog-controller-for-blog-name-not-found
  "Test blog controller with blog name that doesn't exist."

  (with-mocks ()
    (answer (blog-repo:repo-get-all) (cons :ok (list *blog-entry*)))
    (answer (blog-repo:repo-get-for-name _)
      (cons :not-found-error "Post 'my blog post' doesn't exist!"))

    (let ((controller-result (controller.blog:for-blog-name "my blog name")))
      (is (equalp (cons :not-found-error "Post 'my blog post' doesn't exist!")
                  controller-result)))
    (is (= 0 (length (invocations 'view.blog:render))))
    (is (= 0 (length (invocations 'blog-repo:repo-get-all))))
    (is (= 1 (length (invocations 'blog-repo:repo-get-for-name))))))


;; -----------------------------------
;; blog controller - atom ------------
;; -----------------------------------

(test blog-controller-atom-feed
  "Test generating the atom feed."

  (setf *blog-entry*
        (blog-repo:make-blog-entry "my_blog_name"
                                   (get-universal-time)
                                   "<b>hello world</b>"))

  (with-mocks ()
    (answer (blog-repo:repo-get-all) (cons :ok (list *blog-entry*)))
    (answer (atom-feed:generate-feed feed-model)
      (if (not (hash-table-p feed-model))
          (cons :nok "Wrong input type")
          (cons :ok "<feed")))

    (let ((controller-result (controller.blog:atom-feed)))
      (is (eq :ok (car controller-result)))
      (print controller-result)
      (print (cdr controller-result))
      (is (string= "<feed" (cdr controller-result))))

    (is (= 1 (length (invocations 'blog-repo:repo-get-all))))
    (is (= 1 (length (invocations 'atom-feed:generate-feed))))))

(test blog-controller-atom-feed--nok-in-blog-repo
  "Test error result from blog-repo"

  (with-mocks ()
    (answer (blog-repo:repo-get-all) (cons :nok "Foo"))

    (let ((controller-result (controller.blog:atom-feed)))
      (is (eq :nok (car controller-result)))
      (is (string= "Foo" (cdr controller-result))))

    (is (= 1 (length (invocations 'blog-repo:repo-get-all))))
    (is (= 0 (length (invocations 'atom-feed:generate-feed))))))

(test blog-controller-atom-feed--nok-in-generate-atom-feed
  "Test error result from atom generation"

  (setf *blog-entry*
        (blog-repo:make-blog-entry "my_blog_name"
                                   (get-universal-time)
                                   "<b>hello world</b>"))
  (with-mocks ()
    (answer (blog-repo:repo-get-all) (cons :ok (list *blog-entry*)))
    (answer (atom-feed:generate-feed _) (cons :nok "Bar"))

    (let ((controller-result (controller.blog:atom-feed)))
      (is (eq :nok (car controller-result)))
      (is (string= "Bar" (cdr controller-result))))

    (is (= 1 (length (invocations 'blog-repo:repo-get-all))))
    (is (= 1 (length (invocations 'atom-feed:generate-feed))))))

(defun run-tests ()
  (run! 'index-controller)
  (run! 'imprint-controller)
  (run! 'about-controller)

  (run! 'blog-controller-index)
  (run! 'blog-controller-index-no-blog-entry)
  (run! 'blog-controller-for-blog-name)
  (run! 'blog-controller-for-blog-name-not-found)

  (run! 'blog-controller-atom-feed)
  (run! 'blog-controller-atom-feed--nok-in-blog-repo)
  (run! 'blog-controller-atom-feed--nok-in-generate-atom-feed))
