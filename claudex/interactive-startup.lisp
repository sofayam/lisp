;; Load Quicklisp (if not auto-loaded)
(load "~/quicklisp/setup.lisp")

;; Install dependencies (first time only)
(ql:quickload :hunchentoot)
(ql:quickload :sqlite)
(ql:quickload :cl-who)
(ql:quickload :fiveam)

;; Load your system
(asdf:load-asd "/Users/mark/repos/lisp/claudex/claudex.asd")
(ql:quickload :claudex)


(push #P"/Users/mark/repos/lisp/claudex/" asdf:*central-registry*)

;; Start the server
(in-package :claudex)
(start-server)
