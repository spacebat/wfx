(in-package :wfx-examples)

;;Create and render simle widget

(defclass simple-widget-dom (widget)
  ((simple-property)
   (submit-status)))

(defmethod render ((instance simple-widget-dom) &rest named-pairs &key  &allow-other-keys)
  (declare (ignore named-pairs))   
  (with-html-output-to-string (*standard-output*)     
    (:form :action "" :method "post" :name "simple_post"
	   (:input :type "text" 
		   :name (wfx::widgy-name instance "simple-property") 
		   :value (if (slot-boundp instance 'simple-property)
			      (str (slot-value instance 'simple-property))
			      ""))
	   (:input :type "submit" :name "submit" :value "submit"))

    (if  (slot-boundp instance 'simple-property)
	 (str (format nil "The value of the slot simple-property = ~A" (slot-value instance 'simple-property))))
    (:br)
    (if  (slot-boundp instance 'submit-status)
	 (str (format nil "The submit button has been presse ~A times so far." (slot-value instance 'submit-status))))
    ))


(defmethod action-handler ((instance simple-widget-dom))
  (let ((count (if (slot-boundp instance 'submit-status) 
		   (if (slot-value instance 'submit-status)
			     (slot-value instance 'submit-status)
			     0)
		   0)))
    (if (parameter "submit")
	(setf (slot-value instance 'submit-status) (incf count) ))))


(define-easy-handler (example-simple-dom :uri "/wfx/examples/simple-dom.html") ()
  (let* ((page (make-widget 'wfx-page :instance-name "example-simple-dom"  :header-text "WFX" :sub-text "How the dom works for a simple widget."
			      :paging (list "/wfx/examples/example-index.html" "/wfx/examples/simple.html" "/wfx/examples/more-dom.html"))))

    (with-html-output-to-string (*standard-output*)
      (str (render page 
		   :body (with-html-output-to-string (*standard-output*)
			   (:p "This example demonstrates how WFX updates a widget in the dom in a post event.")

			   (:pre "(defwidget simple-widget-dom (widget)
  ((simple-property))
  (:metaclass widget-class))")
      
			   (:pre "(defmethod render ((widget simple-widget-dom) &rest named-pairs &key  &allow-other-keys)
  (declare (ignore named-pairs))   
  (with-html-output-to-string (*standard-output*)     
    (:form :action \"\" :method \"post\" :name \"simple_post\"
	   (:input :type \"text\" 
		   :name (widgy-name widget \"simple-property\") 
		   :value (if (slot-boundp widget 'simple-property)
			      (str (slot-value widget 'simple-property))
			      \"\"))
	   (:input :type \"submit\" :name \"submit\" :value \"submit\"))
    (if  (slot-boundp widget 'simple-property)
	 (htm (str (format nil \"The value of the slot simple-property = ~A\" (slot-value widget 'simple-property)))))))")

			   (:pre "(define-easy-handler (example-simple-dom :uri \"/ht-wfx/example-simple-dom.html\") ()
  (let ((my-widget (make-widget 'simple-widget-dom :name \"my-widget\")))
    (with-html-output-to-string (*standard-output*)
      (render my-widget))))" )
      
			   (:p "In this example the widget's slot, named \"simple-property\", is initialy unbound, but once the submit button is pressed the \"simple-property\" slot value is set to the value of the textbox. This happens because by default widget slots that have names that match Hunchentoot request parameter names are updated with the parameter's value before the request is passed to its handler.")

			   (:p "To understand how the above rule translates to this example's behaviour take note of the following:"
			       (:ul
				(:li "The textbox has the widgyfied* name of \"simple-property\" which matches the widget slot name \"simple-property\".")
				(:li "The textbox is wrapped in an html form which is posted by the submit button.")
				(:li "This translates to a Hunchentoot request parameter of the widgyfied name \"simple-property\" the value of which is the text contained in the textbox at the time of the submit.")
				(:li "The value of the widget's slot, named \"simple-property\" is set to the value of the Hunchentoot request parameter of the same widgyfied name, before the request is passed to its handler.")	   
				(:li "The slot's value is written out in a message to prove that the value is now = to the text in the textbox.")
				))
      
			   (:p "* The textbox's name is widgyfied to ensure that a request parameter is macthed to the correct instance of the widget.")
      
			   (:hr)     
			   (:br)
			   (str (render (make-widget 'simple-widget-dom :instance-name "my-widget")))))))))


