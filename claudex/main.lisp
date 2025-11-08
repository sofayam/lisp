(in-package #:claudex)

(defparameter *server* nil)
(defparameter *db-path* "test.db")

(defun init-db ()
  "Initialize SQLite database with sample data"
  (sqlite:with-open-database (db *db-path*)
    (sqlite:execute-non-query 
     db "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT, email TEXT)")
    (sqlite:execute-non-query 
     db "INSERT OR IGNORE INTO users (id, name, email) VALUES (1, 'Alice', 'alice@example.com')")
    (sqlite:execute-non-query 
     db "INSERT OR IGNORE INTO users (id, name, email) VALUES (2, 'Bob', 'bob@example.com')")))

(defun get-all-users ()
  "Fetch all users from database"
  (sqlite:with-open-database (db *db-path*)
    (sqlite:execute-to-list db "SELECT id, name, email FROM users")))

(define-easy-handler (home-page :uri "/") ()
  (setf (content-type*) "text/html")
  (with-html-output-to-string (s)
    (:html
     (:head (:title "SQLite Web Viewer"))
     (:body
      (:h1 "Users Database")
      (:table :border "1"
       (:tr (:th "ID") (:th "Name") (:th "Email"))
       (dolist (user (get-all-users))
         (htm
          (:tr
           (:td (str (first user)))
           (:td (str (second user)))
           (:td (str (third user)))))))))))

(defun start-server (&optional (port 8080))
  "Start the web server"
  (when *server*
    (stop-server))
  (init-db)
  (setf *server* (make-instance 'easy-acceptor :port port))
  (start *server*)
  (format t "Server started on http://localhost:~a~%" port))

(defun stop-server ()
  "Stop the web server"
  (when *server*
    (stop *server*)
    (setf *server* nil)
    (format t "Server stopped~%")))
