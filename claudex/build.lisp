
(push #P"./" asdf:*central-registry*)
(ql:quickload :claudex)

(defun main ()
  "Entry point for the standalone binary"
  (claudex:start-server)
  ;; Keep the server running - wait forever
  (handler-case 
      (loop (sleep 1))
    ;; Exit gracefully on Ctrl-C
    (sb-sys:interactive-interrupt ()
      (format t "~%Shutting down...~%")
      (claudex:stop-server)
      (sb-ext:exit :code 0))))

(sb-ext:save-lisp-and-die "claudex"
  :toplevel #'main
  :executable t
  :compression t)
