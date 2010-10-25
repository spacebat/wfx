(in-package :wfx-examples)

(defclass wfx-header (widget)
  ((header-text :initarg :header-text :accessor header-text)
   (sub-text :initarg :sub-text :accessor sub-text)
   (paging :initarg :paging :initform nil :accessor paging)))

(defmethod render ((widget wfx-header) &rest named-pairs &key  &allow-other-keys)  
  (declare (ignore named-pairs))
  (with-html-output-to-string (*standard-output* nil :indent t)    
    (:div :style "font-weight:bold;font-size:16px"
	  (:h2 (str (header-text widget)))
	    
	  (if (slot-boundp widget 'sub-text)
	      (if (sub-text widget)
		  (htm 
		   (:h1 (str (sub-text widget))))))
	    
	  (if (slot-boundp widget 'paging)
	      (if (paging widget)
		  (htm (:p 
			(:a :style "text-decoration:none" :href (first (paging widget)) "INDEX ") 
			(:a :style "text-decoration:none" :href (second (paging widget)) "PREVIOUS " )
			(:a :style "text-decoration:none" :href (third (paging widget)) "NEXT" ))))))))

(defclass wfx-footer (widget)
  ())

(defmethod render ((widget wfx-footer) &rest named-pairs &key  &allow-other-keys) 
  (declare (ignore named-pairs)) 
  (with-html-output-to-string (*standard-output* nil :indent t)     
    (:div :style "position: relative; margin: 0 auto; text-align: left;width:100%;"
	  (:br)
	  (:div  :style "position:absolute;top:0em;left:auto;z-index:3;font-weight:bold;width:100%;" 
		 (:hr :style "width:100%")
		 (str "WFX is ") (:a :href "/wfx/examples/license.html" "licensed") (str " under ") 
		 (:a :href "http://www.opensource.org/licenses/bsd-license.php" "The BSD License"))
	  (:div :style "position:absolute;top:-2.3em;right:0em;z-index:3" 

		(:img :src "/wfx/examples/images/lisp-logo-small.png" )
		(:img :src "/wfx/examples/images/ediware-logo-small.png" )
		(:img :src "/wfx/examples/images/logo-small.jpg" )

		))))



(defclass wfx-page (widget)
  ((header-text :initarg :header-text :accessor header-text)
   (sub-text :initarg :sub-text :accessor sub-text)
   (paging :initarg :paging :initform nil :accessor paging))
  (:metaclass widget-class)
  (:include-bits "<link rel=\"stylesheet\" type=\"text/css\" href=\"./css/style.css\"/>" ))


(defmethod render ((instance wfx-page) &rest named-pairs &key body &allow-other-keys)   
  (declare (ignore named-pairs))  
  (let ((header (make-widget 'wfx-header :instance-name "header"))
	(footer (make-widget 'wfx-footer :instance-name "footer")))

    (setf (header-text header) (header-text instance))
    (setf (sub-text header) (sub-text instance))
    (setf (paging header) (paging instance))

    (with-html-output-to-string (*standard-output* nil :indent t) 
      (:html 
       (:head 
	(:title "DATA X-WARE")
	
	(:meta :name "keywords" :content "")
	(:meta :name "Author" :content "DATA X-WARE; info@dataxware.co.za")
	(:meta :name "Classification" :content "")
	(:meta :name "Description" :content "")

	(wfx::page-include-bits))

       (:body :class "bodyclass"  
	      (:div
	       (:table :cellspacing 0 :cellpadding 0 :style "position:relative;text-align:center;margin: 0 auto;border:1px;cell-padding:0;cell-spacing:0;"
			 
		       (:tr
			(:td 
			 (str (render header))))

		       (:tr 
			(:td :style "text-align:left"
			     (str body)))

		       (:tr
			(:td 
			 (str (render footer)))))))))))