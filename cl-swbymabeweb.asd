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
               

               ;; for @route annotation
               "cl-syntax-annot"

               ;; html templating
               "cl-who")
  :components ((:module "src"
                :components
                ((:file "config")
                 (:file "blog-repo")
                 (:file "main")
                 (:file "views/common")
                 (:file "views/index")
                 (:file "views/imprint")
                 (:file "views/about")
                 (:file "views/blog")
                 (:file "controllers/common")
                 (:file "controllers/index" :depends-on ("controllers/blog"))
                 (:file "controllers/imprint")
                 (:file "controllers/about")
                 (:file "controllers/blog")
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
                 (:file "it-routing" :depends-on ("all-tests"))
                 )))
  :description "Test system for cl-swbymabeweb"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:cl-swbymabeweb.tests))))


;; (push #P"~/Development/MySources/cl-swbymabeweb/" asdf:*central-registry*)
;; (asdf:load-system "cl-swbymabeweb")
;; (asdf:test-system "cl-swbymabeweb/tests")


#|
TODOs:

- blog-agent must be able to deal with blog-names with space characters

|#
