(defsystem "cl-swbymabeweb"
  :version "0.1.0"
  :author "Manfred Bergmann"
  :license ""
  :depends-on ("clack"
               "lack"
               "caveman2"
               "envy"
               "cl-ppcre"
               "uiop"
               "log4cl"
               "cl-locale"
               "local-time"
               "cl-date-time-parser"
               "serapeum"
               "uiop"
               "str"
               "3bmd"
               "3bmd-ext-code-blocks"
               "xml-emitter"
               

               ;; for @route annotation
               "cl-syntax-annot"

               ;; html templating
               "cl-who")
  :components ((:module "src"
                :components
                ((:file "config")
                 (:file "blog-repo")
                 (:file "atom-feed-util")
                 (:file "main")
                 (:module "views"
                  :components
                          ((:file "common")
                           (:file "index")
                           (:file "imprint")
                           (:file "about")
                           (:file "blog")))
                 (:module "controllers"
                  :components
                          ((:file "common")
                           (:file "index" :depends-on ("blog"))
                           (:file "imprint")
                           (:file "about")
                           (:file "blog")))
                 (:file "web"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-swbymabeweb/tests"))))


(defsystem "cl-swbymabeweb/tests"
  :author "Manfred Bergmann"
  :license ""
  :depends-on ("cl-swbymabeweb"
               "fiveam"
               "cl-mock"
               "dexador"
               "str")
  :components ((:module "tests"
                :components
                ((:file "all-tests")
                 (:file "controller-test" :depends-on ("all-tests"))
                 (:file "view-test" :depends-on ("all-tests"))
                 (:file "blog-repo-test" :depends-on ("all-tests"))
                 (:file "atom-feed-util-test" :depends-on ("all-tests"))
                 (:file "it-routing" :depends-on ("all-tests"))
                 )))
  :description "Test system for cl-swbymabeweb"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:cl-swbymabeweb.tests))))


;; (asdf:load-system "cl-swbymabeweb")
;; (asdf:test-system "cl-swbymabeweb/tests")


#|
TODOs:

|#
