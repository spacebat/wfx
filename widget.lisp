(in-package :wfx)

(defclass widget-class (standard-class)
  ((include-bits :initarg :include-bits :initform nil :accessor include-bits))
  (:documentation "Meta class for widgets."))

(defmethod validate-superclass ((class widget-class)
                                (superclass standard-class))
  t)

(defmethod validate-superclass ((superclass standard-class)
                                (class widget-class))
  t)

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

(defclass widget ()
  ((name :initarg :name
         :initform nil
         :accessor name)
   (group-index :initarg :group-index
                :initform nil :accessor group-index)
   (data :initarg :data
         :initform nil
         :accessor data)))

(defgeneric render (widget &rest named-pairs &key &allow-other-keys)
  (:documentation "Renders a widget."))

(defun to-html (string)
  (ppcre:regex-replace "-" string "_"))

(defun from-html (string)
  (ppcre:regex-replace  "_" string "-"))

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

(defun make-widget (widget-class &rest args
                    &key name group-index &allow-other-keys)
  "This function instanciates a widget or returns the widget from the dom if it already exists.
Each request uri has its own hashtable with widgets. The hashtable represents a simple dom.
The dom is automatically updated before a request is passed to a hunchentoot handler."
  (let* ((class-name (class-name
                      (etypecase widget-class
                        (symbol (find-class widget-class))
                        (class widget-class))))
         (name (format nil "~a~@[-~a~]" class-name name))
         (cache (cache))
         (instance (gethash name cache)))
    (cond ((not instance)
           (setf instance (apply #'make-instance widget-class
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
    instance))

(defun get-method (name)
  (when (fboundp name)
    (let ((generic-function (fdefinition (list 'setf name))))
      (when generic-function
        (compute-discriminating-function generic-function)))))

(defun update-slot (instance slot-name value)
  (let ((method (get-method slot-name)))
    (if method
        (funcall method value instance)
        (setf (slot-value instance slot-name) value))))

(defgeneric synq-widget-data (widget))

(defgeneric action-handler (widget)
  (:documentation "This function is called after the dom has been updated from parameters.
This is the ideal place to place code that handles post or get actions.")
  (:method ((widget widget))))

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
              do (update-slot object (slot-definition-name slot) value))))))

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
    (map-dom (lambda (value)
               (update-dom value)
               (synq-widget-data value)))
    (map-dom #'action-handler)
    (setf (dom) ())))

(defun widget-include-bits (widget-class-instance)
  "Returns the include statements for then widget's include files."
  (when widget-class-instance
    (map nil #'princ (include-bits widget-class-instance))))

(defun page-include-bits ()
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

(defun set-widget (instance &key group-index )
  "This function instanciates a widget or returns the widget from the dom if it already exists.
Each request uri has its own hashtable with widgets. The hashtable represents a simple dom.
The dom is automatically updated before a request is passed to a hunchentoot handler."
  (let ((cache (cache)))
    (if group-index
        (setf (gethash (name instance) cache)
              (make-hash-table :test 'equal)
              (gethash group-index (gethash (name instance) cache))
              instance)
        (setf (gethash (name instance) cache) instance))
    instance))
