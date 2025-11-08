(asdf:defsystem #:claudex
  :description "Simple SQLite web viewer"
  :author "Your Name"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:hunchentoot
               #:cl-sqlite
               #:cl-who)
  :components ((:file "package")
               (:file "main" :depends-on ("package")))
  :in-order-to ((test-op (test-op "claudex/tests"))))

(asdf:defsystem #:claudex/tests
  :depends-on (#:claudex
               #:fiveam)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :fiveam :run! :claudex-tests)))
