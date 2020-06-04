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
                 (:file "controller/index-controller")
                 (:file "web")
                 (:file "view" :depends-on ("config"))
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-swbymabeweb-test"))))


;; (push #P"~/Development/MySources/cl-swbymabeweb/" asdf:*central-registry*)
;; (asdf:load-system "cl-swbymabeweb")
;; (asdf:load-system "cl-swbymabeweb-test")
