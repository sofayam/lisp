(defpackage #:calculate
  (:use #:cl)
  (:nicknames #:ca)
  (:export #:calc))

(in-package #:calculate)

(defun calc (n)
  (* n n))