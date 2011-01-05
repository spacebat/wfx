(in-package :common-lisp-user)

(defpackage :wfx
  (:nicknames :wfx)
  (:use :cl :sb-mop :hunchentoot :cl-who :s-sql :date-calc)
  (:export
   :widget-class
   :widget
   :html-element
   :widgy-name
   :un-widgy-name
   :render
   :action-handler
   :synq-widget-data
   :name
   :make-widget
   :page-include-bits
   :get-widget
   :set-widget))


