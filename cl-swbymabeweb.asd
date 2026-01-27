(defsystem "cl-swbymabeweb"
  :version "0.3.0"
  :author "Manfred Bergmann"
  :license ""
  :depends-on ("alexandria"
               "hunchentoot"
               "snooze"
               "envy"
               "cl-ppcre"
               "uiop"
               "log4cl"
               "cl-i18n"
               "local-time"
               "cl-date-time-parser"
               "serapeum"
               "uiop"
               "str"
               "3bmd"
               "3bmd-ext-code-blocks"
               "3bmd-ext-tables"
               "xml-emitter"
               "cl-who"
               "sento")
  :components ((:module "src"
                :components
                ((:file "config")
                 (:file "blog-repo")
                 (:file "atom-feed")
                 (:module "views"
                  :components
                          ((:file "common")
                           (:file "index")
                           (:file "imprint")
                           (:file "about")
                           (:file "projects")
                           (:file "blog")))
                 (:module "controllers"
                  :components
                          ((:file "common")
                           (:file "index" :depends-on ("blog"))
                           (:file "imprint")
                           (:file "about")
                           (:file "projects")
                           (:file "blog")))
                 (:file "routes")
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-swbymabeweb/tests"))))


(defsystem "cl-swbymabeweb/tests"
  :author "Manfred Bergmann"
  :license ""
  :depends-on ("cl-swbymabeweb"
               "fiveam"
               "cl-mock"
               "dexador")
  :components ((:module "tests"
                :components
                ((:file "all-tests")
                 (:file "routes-test" :depends-on ("all-tests"))
                 (:file "controller-test" :depends-on ("all-tests"))
                 (:file "view-test" :depends-on ("all-tests"))
                 (:file "blog-repo-test" :depends-on ("all-tests"))
                 (:file "atom-feed-test" :depends-on ("all-tests"))
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

extracting string resources:
(cl-i18n-utils:gen-translation-file "src/views/" "i18n/english.lisp")

|#
