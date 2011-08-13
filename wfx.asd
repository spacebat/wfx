(defsystem wfx
  :name "wfx"
  :version "0.1"
  :depends-on (hunchentoot closer-mop)
  :serial t
  :components ((:file "defpackage")
               (:file "ini")
               (:file "widget")))
