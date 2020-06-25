(defpackage :cl-swbymabeweb.atom-feed-test
  (:use :cl :fiveam :cl-mock :local-time :cl-swbymabeweb.atom-feed)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-swbymabeweb.atom-feed-test)

(def-suite atom-feed-tests
  :description "Tests the atom feed generation."
  :in cl-swbymabeweb.tests:test-suite)

(in-suite atom-feed-tests)

(defparameter *feed-header* (make-atom-feed-header
                             :title "Foo"
                             :author "Manfred Bergmann"
                             :link "http://foo/"
                             :id "urn:foobar"
                             :updated "2020-06-23"))

(defparameter *feed-entry-1* (make-atom-feed-entry
                              :title "My first post"
                              :link "http://foo1/"
                              :updated "2020-06-23"
                              :id "http://id1"
                              :content "My content"))

(defparameter *feed-entry-2* (make-atom-feed-entry
                              :title "My second post"
                              :link "http://foo2/"
                              :updated "2020-06-22"
                              :id "http://id2"
                              :content "My second content"))

(defparameter *feed-model-2* (make-atom-feed-model *feed-header*
                                                   (list *feed-entry-1* *feed-entry-2*)))

(test atom-feed-generate-empty
  "Test the atom feed generation."

  (let ((atom-feed (cdr
                    (atom-feed:generate-feed (make-atom-feed-model (make-hash-table) nil)))))
    (is (str:starts-with-p "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">" atom-feed))
    (is (str:ends-with-p "</feed>" atom-feed))))

(test atom-feed-generate-with-header
  "Test the atom feed generation with header."

  (let ((atom-feed (cdr
                    (atom-feed:generate-feed (make-atom-feed-model *feed-header* nil)))))
    (is (str:containsp "<title>Foo</title>" atom-feed))
    (is (str:containsp "<author>" atom-feed))
    (is (str:containsp "<name>Manfred Bergmann" atom-feed))
    (is (str:containsp "</author>" atom-feed))
    (is (str:containsp "<link href=\"http://foo/\"></link>" atom-feed))
    (is (str:containsp "<id>urn:foobar</id>" atom-feed))
    (is (str:containsp "<updated>2020-06-23</updated>" atom-feed))))

(test atom-feed-generate-with-items
  "Test atom feed generation with items"
  (let ((atom-feed (cdr
                    (atom-feed:generate-feed (make-atom-feed-model
                                             (make-hash-table)
                                             (list *feed-entry-1*))))))
    (is (str:containsp "<entry>" atom-feed))
    (is (str:containsp "</entry>" atom-feed))
    (is (str:containsp "<title type=\"html\"><![CDATA[ My first post ]]></title>" atom-feed))
    (is (str:containsp "<link href=\"http://foo1/\"></link>" atom-feed))
    (is (str:containsp "<updated>2020-06-23</updated>" atom-feed))
    (is (str:containsp "<id>http://id1</id>" atom-feed))
    (is (str:containsp "<content type=\"html\"><![CDATA[ My content ]]></content>" atom-feed))
  ))

(test atom-feed-generate-with-2-items
  "Test atom feed generation with 2 items"
  (let ((atom-feed (cdr
                    (atom-feed:generate-feed *feed-model-2*))))
    (is (str:containsp "<title type=\"html\"><![CDATA[ My first post ]]></title>" atom-feed))
    (is (str:containsp "<title type=\"html\"><![CDATA[ My second post ]]></title>" atom-feed))
    (is (str:containsp "<link href=\"http://foo1/\"></link>" atom-feed))
    (is (str:containsp "<link href=\"http://foo2/\"></link>" atom-feed))
    (is (str:containsp "<updated>2020-06-23</updated>" atom-feed))
    (is (str:containsp "<updated>2020-06-22</updated>" atom-feed))
    (is (str:containsp "<id>http://id1</id>" atom-feed))
    (is (str:containsp "<id>http://id2</id>" atom-feed))
    (is (str:containsp "<content type=\"html\"><![CDATA[ My content ]]></content>" atom-feed))
    (is (str:containsp "<content type=\"html\"><![CDATA[ My second content ]]></content>" atom-feed))
  ))

(defun run-all ()
  (run! 'atom-feed-generate-empty)
  (run! 'atom-feed-generate-with-header)
  (run! 'atom-feed-generate-with-items)
  (run! 'atom-feed-generate-with-2-items))
