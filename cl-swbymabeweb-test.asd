(defsystem "cl-swbymabeweb-test"
  :author "Manfred Bergmann"
  :license ""
  :depends-on ("cl-swbymabeweb"
               "fiveam"
               "cl-mock"
               "dexador"
               "str")
  :components ((:module "tests"
                :components
                ((:test-file "it-routing")
                 (:test-file "controller-test"))))
  :description "Test system for cl-swbymabeweb"
  :perform (test-op (op c) (symbol-call :fiveam :run! 'tests)))
