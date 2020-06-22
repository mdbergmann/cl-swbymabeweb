(defpackage :cl-swbymabeweb.atom-feed
  (:use :cl :xml-emitter)
  (:nicknames :atom-feed)
  (:export #:generate-feed))

(in-package :cl-swbymabeweb.atom-feed)


(defun generate-feed (blog-posts))
