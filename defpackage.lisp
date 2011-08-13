(in-package :common-lisp-user)

(defpackage :wfx
  (:use :c2cl hunchentoot)
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
   :set-widget
   :find-slot
   :data
   :css-class
   :style
   :to-html
   :from-html))


