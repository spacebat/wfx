(in-package :common-lisp-user)

(defpackage :wfx
  (:nicknames :wfx)
  (:use :cl :sb-mop :hunchentoot :cl-who :s-sql :date-calc)
  (:export
   :widget-class
   :widget
   :render
   :name
   :make-widget)
  )


