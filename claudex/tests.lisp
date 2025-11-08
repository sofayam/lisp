(defpackage #:claudex/tests
  (:use #:cl #:fiveam #:claudex))

(in-package #:claudex/tests)

(def-suite claudex-tests
  :description "Tests for claudex")

(in-suite claudex-tests)

(test database-initialization
  "Test that database initializes correctly"
  (claudex::init-db)
  (let ((users (claudex::get-all-users)))
    (is (>= (length users) 2))
    (is (equal "Alice" (second (first users))))))

(test get-users
  "Test fetching users"
  (let ((users (claudex::get-all-users)))
    (is (listp users))
    (is (every #'listp users))))
