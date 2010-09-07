(in-package :wfx-examples)

(defun mapc-directory-tree (fn directory)
  (dolist (entry (cl-fad:list-directory directory))
    (when (cl-fad:directory-pathname-p entry)
      (mapc-directory-tree fn entry))
    (funcall fn entry)))


(mapc-directory-tree (lambda (x)
		       (when (or (equal (pathname-type x) "js") (equal (pathname-type x) "css") (equal (pathname-type x) "png") (equal (pathname-type x) "jpg")
				 (equal (pathname-type x) "gif") (equal (pathname-type x) "htm") (equal (pathname-type x) "pdf"))

			 (push (create-static-file-dispatcher-and-handler 
				(format nil "/~A" (wfx::replace-all (format nil "wfx/examples/~A" (pathname x)) "/home/phil/Development/dataxware/wfx/examples/" "")) 
				(format nil "~A" (pathname x))
				) *dispatch-table*)))
		     "/home/phil/Development/dataxware/wfx/examples/")