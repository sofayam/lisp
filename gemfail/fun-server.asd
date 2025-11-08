(asdf:defsystem #:fun-server
  :description "A simple lisp server."
  :author "Mark"
  :license "MIT"
  :depends-on (#:clack #:ningle)
  :serial t
  :components ((:file "calc")
               (:file "fun"))
  :build-operation "program-op"
  :entry-point "markpack:start-server")