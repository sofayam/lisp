;;;; start.lisp - Development startup script for claudex

;; Load Quicklisp
;;(load "~/quicklisp/setup.lisp")

;; Add this project to ASDF's search path temporarily
(push #P"./" asdf:*central-registry*)

;; Load the system
(ql:quickload :claudex)

;; Switch to the package
(in-package :claudex)

;; Initialize and start the server
(format t "~%Starting Claudex server...~%")
(start-server)

(format t "~%Claudex is ready! Visit http://localhost:8080~%")
(format t "To stop: (stop-server)~%~%")
