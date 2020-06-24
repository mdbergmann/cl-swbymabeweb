#|

I first thought to use the macros in the controller.
But that shifted the feed generation and tests thereof to the controller, 
which it is not responsible for.
Rather it should map together data for the feed generation and let the feed generator do it's work.

|#

(defpackage :cl-swbymabeweb.atom-feed
  (:use :cl :xml-emitter)
  (:nicknames :atom-feed)
  (:export #:generate-feed
           #:make-atom-feed-model
           #:make-atom-feed-header
           #:make-atom-feed-entry))

(in-package :cl-swbymabeweb.atom-feed)

(defun make-atom-feed-model (header entries)
  "Makes a atom-feed-model that is used to generate the atom xml feed.
`header': the header. make it with `make-atom-feed-header'
`entries': is a list of items created using `make-atom-feed-entry'"
  (let ((map (make-hash-table)))
    (setf (gethash :header map) header)
    (setf (gethash :entries map) entries)
    map))

(defun make-atom-feed-header (&key title author link id updated)
  (let ((map (make-hash-table)))
    (setf (gethash :title map) title)
    (setf (gethash :author map) author)
    (setf (gethash :link map) link)
    (setf (gethash :id map) id)
    (setf (gethash :updated map) updated)
    map))

(defun make-atom-feed-entry (&key title link id updated content)
  (let ((map (make-hash-table)))
    (setf (gethash :title map) title)
    (setf (gethash :content map) content)
    (setf (gethash :link map) link)
    (setf (gethash :id map) id)
    (setf (gethash :updated map) updated)
    map))


(defun atom-header (feed-header)
  (emit-simple-tags :title (gethash :title feed-header)
                    :updated (gethash :updated feed-header)
                    :id (gethash :id feed-header))
  (with-tag ("author")
    (emit-simple-tags :name (gethash :author feed-header)))
  (with-simple-tag ("link" `(("href" ,(gethash :link feed-header))))))

(defun atom-entry (feed-entry)
  (with-tag ("entry")
    (with-simple-tag ("title" '(("type" "html")))
      (xml-as-is (format nil "<![CDATA[ ~a ]]>" (gethash :title feed-entry))))
    (simple-tag "link" "" `(("href" ,(gethash :link feed-entry))))
    (simple-tag "updated" (gethash :updated feed-entry))
    (simple-tag "id" (gethash :id feed-entry))
    (with-simple-tag ("content" '(("type" "html")))
      (xml-as-is (format nil "<![CDATA[ ~a ]]>" (gethash :content feed-entry))))))

(defmacro with-atom-feed (&body body)
  (let ((stream (gensym)))
    `(progn
       (setf ,stream (make-string-output-stream))
       (with-xml-output (,stream :encoding "utf-8")
         (with-tag ("feed" ,''(("xmlns" "http://www.w3.org/2005/Atom")))
           ,@body))
       (get-output-stream-string ,stream))))

(defun generate-feed (feed-model)
  "Generates the atom feed as string. 
Input is a hash-table. Make this hash-table with `make-atom-feed-model'"
  (if feed-model
      (cons :ok (with-atom-feed
                  (atom-header (gethash :header feed-model))
                  (dolist (entry (gethash :entries feed-model))
                    (atom-entry entry))))
      (cons :nok "no input model!")))
