(defpackage #:wfx-example-asd
  (:use :cl :asdf))

(in-package :wfx-example-asd)

(defsystem wfx-examples
  :name "wfx-examples"
  :version "0.1"
  :serial t
  :components ((:file "defpackage")
	       (:file "ini" :depends-on ("defpackage"))
	       (:file "site-widgets" :depends-on ("defpackage"))
               (:file "simple" :depends-on ("defpackage"))


	       (:file "static-files" :depends-on ("defpackage"))

               )
  :depends-on (:hunchentoot
	       :cl-who
	       :wfx
	       ))


