(defpackage #:markpack
  (:use #:cl #:calculate)
  (:export #:start-server))

(in-package #:markpack)

(defvar app (make-instance 'ningle:app))

(setf (ningle:route app "/")
  (lambda (params)
    "Hello world"))

(setf (ningle:route app "/calc/:num")
      (lambda (params)
        (let ((n (parse-integer (cdr (assoc :num params)))))
          (format nil "~a" (calc n)))))

(defun start (&key (server :woo) (address "127.0.0.1") (port 8000))
    (clack:clackup
     app
     :server server
     :address address
     :port port))

(defun start-server ()
  (start :port 8888))