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
  ((instance-name :initarg :instance-name
                  :initform nil
                  :accessor instance-name)
   (group-index :initarg :group-index
                :initform nil :accessor group-index)
   (data :initarg :data
         :initform nil
         :accessor data)))

(defgeneric render (widget &rest named-pairs &key &allow-other-keys)
  (:documentation "Renders a widget."))

(defun widgy-name (instance name)
  (format nil "~A_~A" (replace-all (instance-name instance) "-" "_") name))

(defun un-widgy-name (instance name)
  (replace-all name (format nil "~A_" (replace-all (instance-name instance) "-" "_")) ""))

(defun get-slot (instance slot-name)
  (if (slot-boundp instance slot-name)
      (slot-value instance slot-name)))

;;;;

(defparameter *object-cache* (make-hash-table :test #'equal))

(defun get-cache (session-name)
  (or (gethash session-name *object-cache*)
      (setf (gethash session-name *object-cache*)
            (make-hash-table :test 'equal))))

(defun session-name ()
  (let ((token (or (session-value "session-token")
                   (setf (session-value "session-token") (random 10810)))))
    (format nil "~A-~A"
            token
            (substitute #\- #\.
                        (substitute #\- #\/
                                    (car (split-string (request-uri*) #\?)))))))

(defun make-widget (widget-class &rest args
                    &key instance-name group-index &allow-other-keys)
  "This function instanciates a widget or returns the widget from the dom if it already exists.
Each request uri has its own hashtable with widgets. The hashtable represents a simple dom.
The dom is automatically updated before a request is passed to a hunchentoot handler."
  (let* ((cache (get-cache (session-name)))
         (instance (gethash instance-name cache)))
    (cond ((not instance)
           (setf instance (apply #'make-instance widget-class args))
           (if group-index
               (setf (gethash group-index
                              (setf (gethash instance-name cache)
                                    (make-hash-table :test 'equal)))
                     instance)
               (setf (gethash instance-name cache) instance)))
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

(defun find-slot (slot-name object)
  (find slot-name (class-slots (class-of object))
        :key #'slot-definition-name
        :test #'string-equal))

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
    (loop for (key . value) in parameters
          for slot = (find-slot (un-widgy-name widget key) widget)
          when slot
          do (update-slot widget (slot-definition-name slot) value))))

(defmacro with-debugging (&body body)
  ;; Using this as debugging tool because hunchentoot
  ;; swallows all errors here if set to swallow errors.
  `(handler-bind ((error #'invoke-debugger))
     ,@body))

(defun map-dom (function dom)
  (if (hash-table-p dom)
      (loop for value being the hash-value of dom
            do (map-dom function value))
      (funcall function dom)))

(defmethod handle-request :before ((*acceptor* acceptor) (*request* request))
  "Update widgets in the dom before the request is passed to handler."
  (with-debugging
    (let ((cache (get-cache (session-name))))
      (map-dom (lambda (value)
                 (update-dom value)
                 (synq-widget-data value))
               cache)
      (map-dom #'action-handler cache))))

(defun widget-include-bits (widget-class-instance)
  "Returns the include statements for then widget's include files."
  (when widget-class-instance
    (with-html-output (*standard-output*)
      (dolist (include (include-bits widget-class-instance))
        (str include)))))

(defun page-include-bits ()
  (map-dom
   (lambda (value)
     (when (string-equal (class-name (class-of (class-of value)))
                         "widget-class")
       (widget-include-bits (class-of value))))
   (get-cache (session-name))))

(defun get-widget (instance-name &optional group-index)
  (let* ((cache (get-cache (session-name)))
         (instance (gethash instance-name cache)))
    (if (and group-index instance-name)
        (gethash group-index instance)
        instance)))

(defun set-widget (instance &key group-index )
  "This function instanciates a widget or returns the widget from the dom if it already exists.
Each request uri has its own hashtable with widgets. The hashtable represents a simple dom.
The dom is automatically updated before a request is passed to a hunchentoot handler."
  (let ((cache (get-cache (session-name))))
    (if group-index
        (setf (gethash (instance-name instance) cache)
              (make-hash-table :test 'equal)
              (gethash group-index (gethash (instance-name instance) cache))
              instance)
        (setf (gethash (instance-name instance) cache) instance))
    instance))
