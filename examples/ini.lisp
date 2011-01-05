(in-package :wfx-examples)

(hunchentoot:start (make-instance 'hunchentoot:acceptor :port 9999 :name "wfx-example-acceptor"))

 


;(setf *show-lisp-errors-p* t)
;(declaim (optimize (debug 3)))
