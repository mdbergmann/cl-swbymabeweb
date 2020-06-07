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

               ;; for @route annotation
               "cl-syntax-annot"

               ;; html templating
               "spinneret")
  :components ((:module "src"
                :components
                ((:file "config")
                 (:file "main")
                 (:file "blog")
                 (:file "views/common")
                 (:file "views/index")
                 (:file "views/imprint")
                 (:file "views/about")
                 (:file "views/blog")
                 (:file "controllers/index")
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
                 (:file "it-routing")
                 (:file "controller-test")
                 (:file "view-test")
                 )))
  :description "Test system for cl-swbymabeweb"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:cl-swbymabeweb.tests))))


;; (push #P"~/Development/MySources/cl-swbymabeweb/" asdf:*central-registry*)
;; (asdf:load-system "cl-swbymabeweb")
;; (asdf:test-system "cl-swbymabeweb/tests")
