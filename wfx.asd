(defsystem wfx
  :name "wfx"
  :version "0.1"
  :depends-on (:hunchentoot
               :cl-who
               :postmodern
               :s-sql
               :date-calc)
  :serial t
  :components ((:file "defpackage")
               (:file "ini")
               (:file "common")
               (:file "widget")))
