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

               ;; for @route annotation
               "cl-syntax-annot"

               ;; html templating
               "spinneret")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config"))
                 (:file "views/common")
                 (:file "views/index")
                 (:file "controllers/index")
                 (:file "web")
                 (:file "config"))))
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
                 (:file "controller-test"))))
  :description "Test system for cl-swbymabeweb"
  :perform (test-op (op c) (symbol-call :fiveam :run! 'cl-swbymabeweb.tests)))


;; (push #P"~/Development/MySources/cl-swbymabeweb/" asdf:*central-registry*)
;; (asdf:load-system "cl-swbymabeweb")
;; (asdf:test-system "cl-swbymabeweb/tests")
