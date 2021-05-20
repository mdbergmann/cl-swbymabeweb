(defpackage :cl-swbymabeweb.controller.projects
  (:use :cl :controller)
  (:nicknames :controller.projects)
  (:export #:index)
  (:import-from #:serapeum
                #:->))

(in-package :cl-swbymabeweb.controller.projects)

(-> index () controller-result)
(defun index ()
  "Just calls `projects' view's `render' function."
  (log:debug "Projects controller.")
  (cons :ok (view.projects:render
             (lambda () (load-content-resource #P"projects.md")))))
