(in-package :wfx)

(defclass widget-class (standard-class)
  ((include-bits 
    :initarg :include-bits
    :initform nil
    :accessor include-bits
    :documentation "Stuff that needs to be written to the html header to make a widget work. Like a style block or javascript block. See page-include-bits method.")
   (include-js :initarg :include-js
               :initform nil
               :accessor include-js)
   (include-css :initarg :include-css
               :initform nil
               :accessor include-css)
   (action-params 
    :initarg :action-params
    :initform nil
    :accessor action-params
    :documentation "The names of parameters that should trigger handle actions. If you want a widget to be reused make these \"interesting\" and use widgy-name to create the names of these parameters in the render method of the widget."))
  (:documentation "Meta class for widgets."))

(defmethod validate-superclass ((class widget-class)
                                (superclass standard-class))
  t)

(defmethod validate-superclass ((superclass standard-class)
                                (class widget-class))
  t)

(defun all-widget-superclasses (class)
  (labels ((all-superclasses (class)
             (cons class
                   (loop for super in (class-direct-superclasses class)
			 when (typep super 'widget-class)
			 append (all-superclasses super)))))
    (all-superclasses class)))

(defun propage-include-bits (class)
  (let ((superclasses (all-widget-superclasses class)))
    (setf (include-bits class)
          (alexandria:mappend #'include-bits superclasses))
    (setf (include-js class)
          (alexandria:mappend #'include-js superclasses))
    (setf (include-css class)
          (alexandria:mappend #'include-css superclasses))))

(defmethod initialize-instance :after ((class widget-class) &key)
  (propage-include-bits class))

(defmethod reinitialize-instance :after ((class widget-class) &key)
  (propage-include-bits class))

(defclass html-element ()
  ((name :initarg :name
         :initform nil)
   (id :initarg :id
       :initform nil
       :accessor id)
   (css-class :initarg :css-class
              :initform nil
              :accessor css-class)
   (style :initarg :style
          :initform nil
          :accessor style)))

;;; Prefix slots with % so that the subclasses don't get mixed up when using
;;; the same slot names.
(defclass widget ()
  ((%name :initarg :name
          :initform nil
          :accessor name)
   (%group-index :initarg :group-index
                 :initform nil
                 :accessor group-index)
   (%data :initarg :data
          :initform nil
          :accessor data)))

(defmethod print-object ((object widget) stream)
  (if (name object)
      (print-unreadable-object (object stream :type t :identity t)
        (princ (name object) stream))
      (call-next-method)))

(defgeneric render (widget &rest named-pairs &key &allow-other-keys)
  (:documentation "Renders a widget."))

(defun to-html (string)
  (ppcre:regex-replace-all "-" string "_"))

(defun from-html (string)
  (ppcre:regex-replace-all "_" string "-"))

(defun widgy-name (instance slot-name)
  (format nil "~A.~A"
          (to-html (name instance))
          (to-html slot-name)))

(defun parse-name (name)
  (let ((dot (position #\. name)))
    (when dot
      (values (from-html (subseq name 0 dot))
              (from-html (subseq name (1+ dot)))))))

(defun find-slot (slot-name object)
  (let ((slot (find slot-name (class-slots (class-of object))
                    :key #'slot-definition-name
                    :test #'string-equal)))
    (when slot
      (slot-definition-name slot))))

(defun un-widgy-name (instance name)
  (multiple-value-bind (widget-name slot-name) (parse-name name)
    (when (equal widget-name (name instance))
      (find-slot slot-name instance))))

(defun get-slot (instance slot-name)
  (if (slot-boundp instance slot-name)
      (slot-value instance slot-name)))

;;;;

(defun session-cache ()
  (or (session-value 'cache)
      (setf (session-value 'cache)
            (make-hash-table :test 'equal))))

(defun clear-cache ()
  (setf (session-value 'cache) nil))

(defun cache (&key script-name)
  (let ((script-name (or script-name (script-name*)))
        (session-cache (session-cache)))
    (or (gethash script-name session-cache)
        (setf (gethash script-name session-cache)
              (make-hash-table :test 'equal)))))

(defun doms ()
  (or (session-value 'doms)
      (setf (session-value 'doms)
            (make-hash-table :test 'equal))))
(defun dom ()
  (gethash (script-name*) (doms)))

(defun (setf dom) (value)
  (setf (gethash (script-name*) (doms))
        value))

(defun widget-class (class-or-symbol)
  (etypecase class-or-symbol
    (symbol (find-class class-or-symbol))
    (class class-or-symbol)))

(defun make-widget (widget-class &rest args
                    &key name group-index &allow-other-keys)
  "This function instanciates a widget or returns the widget from the dom if it already exists.
Each request uri has its own hashtable with widgets. The hashtable represents a simple dom.
The dom is automatically updated before a request is passed to a hunchentoot handler."
  (let* ((class (widget-class widget-class))
         (name (or name
                   (string-downcase (class-name class))))
         (cache (cache))
         (instance (gethash name cache)))
    (cond ((not instance)
           (setf instance (apply #'make-instance class
                                 :name name args))
           (if group-index
               (setf (gethash group-index
                              (setf (gethash name cache)
                                    (make-hash-table :test 'equal)))
                     instance)
               (setf (gethash name cache) instance)))
          (group-index
           (unless (setf instance
                         (gethash group-index instance))
             (setf instance (apply #'make-instance widget-class args)
                   (gethash group-index instance) instance))))
    (if (and instance
	     (not (eq (class-of instance) class)))
	(restart-case
	    (error "Widget \"~a\" of class ~a already exist. Requested ~a class."
		   (name instance)
		   (class-name (class-of instance))
		   (class-name class))
	  (replace-widget ()
	    :report "Replace old widget"
	    (setf (gethash name cache) nil)
	    (apply #'make-instance widget-class args)))
	instance)))

(defun find-direct-slot-definition (class slot-name)
  (labels ((find-slot (class)
             (or (find slot-name (class-direct-slots class)
                       :key #'slot-definition-name)
                 (some #'find-slot (class-direct-superclasses class)))))
    (find-slot class)))

(defun find-slot-writer (class slot-name)
  (let ((slot (find-direct-slot-definition class slot-name)))
    (and slot
         (car (slot-definition-writers slot)))))

(defun get-slot-setf-method (object slot-name)
  (let ((writer (find-slot-writer (class-of object)
                                  slot-name)))
    (when writer
      (fdefinition writer))))

(defun update-slot (instance slot-name value)
  (let ((method (get-slot-setf-method instance slot-name)))
    (if method
        (funcall method value instance)
        (setf (slot-value instance slot-name) value))))

(defgeneric synq-widget-data (widget))

(defgeneric action-handler (widget)
  (:documentation "This method is called after the dom has been updated from parameters.
This is the ideal place to place code that handles post or get actions.")
  (:method ((widget widget))))

(defgeneric handle-action (widget action)
  (:documentation "This method is a convenience method that can be used to specialize on actions that are handled by action-handler. For example (defmethod handle-action ((grid grid) (action (eql 'delete))) ...do something here.")
  (:method ((widget widget) action)))

(defmethod action-handler ((widget widget))
  (when (subtypep (class-of (class-of widget)) 'widget-class)
    (if (slot-boundp (class-of widget) ' action-params)
        (dolist (trigger (slot-value (class-of widget) 'action-params))
          (let ((trig (widgy-name widget (string-downcase (symbol-name trigger)))))
            (when (parameter trig)
              (handle-action widget trigger)))))))



(defgeneric update-dom (widget)
  (:documentation "Updates widget slots values.
Slots that have names that match parameter names are updated with the parameter values."))

(defmethod synq-widget-data ((widget widget))
  (let ((parameters (append (get-parameters *request*)
                            (post-parameters *request*))))
    (dolist (object (data widget))
      (when (typep object 'standard-object)
        (loop for (key . value) in parameters
              for slot = (find-slot key object)
              when slot
              do (update-slot object slot value))))))

(defmethod update-dom ((widget widget))
  (let ((parameters (append (get-parameters *request*)
                            (post-parameters *request*))))
    (when (name widget)
      (loop for (key . value) in parameters
            for slot = (un-widgy-name widget key)
            when slot
            do (update-slot widget slot value)))))

(defmacro with-debugging (&body body)
  ;; Using this as debugging tool because hunchentoot
  ;; swallows all errors here if set to swallow errors.
  `(handler-bind ((error #'invoke-debugger))
     ,@body))

(defun map-dom (function)
  (map nil function
       (dom)))

(defmethod render :before ((widget widget) &key)
  (pushnew widget (dom)))

(defmethod handle-request :before ((*acceptor* acceptor) (*request* request))
  "Update widgets in the dom before the request is passed to handler."
  (with-debugging
    (map-dom #'update-dom)
    (map-dom #'synq-widget-data)
    (map-dom #'action-handler)
    
    (setf (dom) ())))


;;TODO: Strip out duplicate includes.

(defun js-inclusion-string (path)
  (format nil "<script type='text/javascript' src='~a'></script>" path))

(defun css-inclusion-string (path)
  (format nil "<link href='~a' rel='stylesheet' type='text/css' />" path))

(defun widget-include-css (widget-class-instance)
  (when widget-class-instance
    (loop for css in (include-css widget-class-instance)
          do (princ (css-inclusion-string css)))))

(defun page-include-css ()
  (map-dom
   (lambda (value)
     (when (subtypep (class-of (class-of value)) 'widget-class)
       (widget-include-css (class-of value))))))

(defun widget-include-js (widget-class-instance)
  (when widget-class-instance
    (loop for js in (include-js widget-class-instance)
          do (princ (js-inclusion-string js)))))

(defun page-include-js ()
  (map-dom
   (lambda (value)
     (when (subtypep (class-of (class-of value)) 'widget-class)
       (widget-include-js (class-of value))))))

(defun widget-include-bits (widget-class-instance)
  "Returns the include statements for then widget's include files."
  (when widget-class-instance
    (map nil #'princ (include-bits widget-class-instance))))

(defun page-include-bits ()
  :documentation "Takes stuff that needs to be written to the html that is independant of the rendering of the actual widget. The method page-include-bits goes through all the widgets for a page on in the dom and adds any include-bits found and this method needs to be explicitly called in html page."
  (map-dom
   (lambda (value)
     (when (subtypep (class-of (class-of value)) 'widget-class)
       (widget-include-bits (class-of value))))))

(defun get-widget (name &key group-index script-name)
  (let* ((cache (cache :script-name script-name))
         (instance (gethash name cache)))
    (if (and group-index name)
        (gethash group-index instance)
        instance)))

(defun set-widget (instance &key group-index)
  "This function instanciates a widget or returns the widget from the dom if it already exists.
Each request uri has its own hashtable with widgets. The hashtable represents a simple dom.
The dom is automatically updated before a request is passed to a hunchentoot handler."
  (let ((cache (cache))
        (name (name instance)))
    (if group-index
        (setf (gethash name cache)
              (make-hash-table :test 'equal)
              (gethash group-index (gethash name cache))
              instance)
        (setf (gethash name cache) instance))
    instance))
