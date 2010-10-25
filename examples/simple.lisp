(in-package :wfx-examples)

(defclass simple-widget (widget)
  ((simple-property)))


(defmethod render ((widget simple-widget) &rest named-pairs &key &allow-other-keys) 
  (declare (ignore named-pairs))   
  (with-html-output-to-string (*standard-output* nil :indent t)           
    (:input :type "text" 
	    :name "simple-propery"
	    :value "A simple widget.")))


(define-easy-handler (example-simple :uri "/wfx/examples/simple.html") ()
  (let* ((my-widget (make-widget 'simple-widget :instance-name "simple-widget"))
	(page (make-widget 'wfx-page :instance-name "simple-pagex" :header-text "WFX" :sub-text "A simple widget."
			     :paging (list "/wfx/examples/example-index.html" "/wfx/examples/index.html" "/wfx/examples/simple-dom.html"))))

    (with-html-output-to-string (*standard-output* nil :indent t) 
      (str (render page :body (with-html-output-to-string (*standard-output* nil :indent t) 
			       (:p "WFX is a simple widget frame work for Hunchentoot. So to do anything with WFX you need a widget.")

			       (:pre "(defclass simple-widget (widget)
  ((simple-property)))")

			       (:p "For a widget to output html it needs a render method. What you see below the line on this page is html output by the following render method.")
      
			       (:pre "(defmethod render ((widget simple-widget) &rest named-pairs &key  &allow-other-keys) 
  (declare (ignore named-pairs))   
  (with-html-output-to-string (*standard-output*)           
    (:input :type \"text\" 
            :name \"simple-propery\"
            :value \"A simple widget.\")))")

			       (:p "To show of the use of the widget it is instanciated and it's render method is called in the following Hunchentoot page.")

			       (:p :style "font-weight:bold;" "Make yourself a big mental note here. The function make-widget does not actually instanciate a new widget each time!. It will use the cached instance from the dom. What this translates to practically is that if you pass different initialization parameters to the widget in a page reload they will not be applied in the make-widget because a dom version already exists. Change the widgets values through accessors or slots if you need to reflect changes since the first instantiation.")
			       (:pre "(define-easy-handler (example-simple :uri \"/wfx/example-simple.html\") ()
  (let ((my-widget (make-widget 'simple-widget :instance-name \"my-widget\")))
    (with-html-output-to-string (*standard-output*)
      (render my-widget))))" )

			       (:hr)      
			       (:br)
			       (str (render my-widget))))))))
    
    

