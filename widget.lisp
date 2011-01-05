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

(defun make-widget (widget-class &rest args)
  "This function instanciates a widget or returns the widget from the dom if it already exists.
Each request uri has its own hashtable with widgets. The hashtable represents a simple dom.
The dom is automatically updated before a request is passed to a hunchentoot handler."
  (let ((instance nil)
        (session-name)
        (widget-name (getf args :instance-name))
        (group-index (getf args :group-index)))
    (unless (session-value "session-token")
      (setf (session-value "session-token") (random 10810)))
    (setf session-name (intern (format nil "~A-~A" (session-value "session-token")
                                       (replace-all (replace-all (car (split-string (request-uri*) #\?)) "/" "-")  "." "-"))))
    (unless (gethash session-name *object-cache*)
      (setf (gethash session-name *object-cache*) (make-hash-table :test 'equal)))
    (when (gethash session-name *object-cache*)
      (when (gethash widget-name (gethash session-name *object-cache*))
        (when group-index
          (setf instance (gethash group-index (gethash widget-name (gethash session-name *object-cache*))))
          (unless instance
            (setf instance (apply #'make-instance widget-class args))
            (setf (gethash group-index (gethash widget-name (gethash session-name *object-cache*))) instance)))
        (unless group-index
          (setf instance (gethash widget-name (gethash session-name *object-cache*)))))
      (unless instance
        (setf instance (apply #'make-instance widget-class args))
        (when group-index
          (setf (gethash widget-name (gethash session-name *object-cache*)) (make-hash-table :test 'equal))
          (setf (gethash group-index (gethash widget-name (gethash session-name *object-cache*))) instance))
        (unless group-index
          (setf (gethash widget-name (gethash session-name *object-cache*)) instance))))
    instance))

(defun get-method (name)
  (when (fboundp name)                  ;'(list 'setf name)
    (let ((generic-function (fdefinition (list 'setf name))))
      (when generic-function
        (compute-discriminating-function generic-function)))))

(defun update-slot (instance slot-name value)
  (let ((method (get-method slot-name)))
    (when method

      (apply method value (list instance)))
    (unless method
      (setf (slot-value instance slot-name) value))))

(defgeneric synq-widget-data (widget)
  (:method ((widget widget))
    (let ((parameters (append (get-parameters *request*) (post-parameters *request*))))
      (when (slot-boundp widget 'data)
        (dolist (object (data widget))
          (when (typep object 'standard-object)
            (let ((slots (class-slots (class-of object))))
              (dolist (parameter parameters)
                (let ((slot (find (car parameter) slots
                                  :key 'slot-definition-name :test #'string-equal)))
                  (when slot
                    (update-slot object (slot-definition-name slot) (cdr parameter))))))))))))

(defgeneric update-dom (widget)
  (:documentation "Updates widget slots values.
Slots that have names that match parameter names are updated with the parameter values.")
  (:method ((widget widget))
    (let ((parameters (append (get-parameters *request*) (post-parameters *request*)))
          (slots (class-slots (class-of widget) )))
      (dolist (parameter parameters)
        (let ((slot (find (un-widgy-name widget (car parameter)) slots
                          :key 'slot-definition-name :test #'string-equal)))
          (when slot
            (update-slot widget (slot-definition-name slot) (cdr parameter))))))))

(defgeneric action-handler (widget)
  (:documentation "This method is called after the dom has been updated from parameters.
This is the ideal place to place code that handles post or get actions.")
  (:method ((widget widget))))

(defmethod handle-request :around ((*acceptor* acceptor) (*request* request))
  "Update widgets in the dom before the request is passed to handler."
  ;;using this as debugging tool because hunchentoot swallows all errors here if set to swallow errors.
  (handler-case
      (let ((session-name (intern (format nil "~A-~A" (session-value "session-token")
                                          (replace-all (replace-all (car (split-string (request-uri*) #\?)) "/" "-")  "." "-")))))
        (when (gethash session-name *object-cache*)
          (loop for v being the hash-value of (gethash session-name *object-cache*)
                do
                (when (hash-table-p v)
                  (maphash
                   #'(lambda (key val)
                       (declare (ignore key))
                       (update-dom val)
                       (synq-widget-data val))
                   v))
                (unless (hash-table-p v)
                  (update-dom v)
                  (synq-widget-data v)))
          (loop for v being the hash-value of (gethash session-name *object-cache*)
                do
                (when (hash-table-p v)
                  (maphash
                   #'(lambda (key val)
                       (declare (ignore key))
                       (action-handler val))
                   v))
                (unless (hash-table-p v)
                  (action-handler v)))))
    (error (e)
      (break (format nil "handle-request :around **** ~A" e))))
  (call-next-method))

(defun widget-include-bits (widget-class-instance)
  "Returns the include statements for then widget's include files."
  (when widget-class-instance

    (dolist (include (include-bits widget-class-instance))

      (with-html-output (*standard-output*)
        (str include)))))

(defun page-include-bits ()
  (let ((session-name (intern (format nil "~A-~A" (session-value "session-token")
                                      (replace-all (replace-all (car (wfx::split-string (request-uri*) #\?)) "/" "-")  "." "-")))))
    (when (gethash session-name *object-cache*)
      (loop for v being the hash-value of (gethash session-name *object-cache*)
            do
            (if (not (hash-table-p v))
                (if (string-equal (class-name (class-of (class-of v))) "widget-class")
                    (widget-include-bits (class-of v)))
                (maphash #'(lambda (key value)
                             (declare (ignore key))
                             (if (string-equal (class-name (class-of (class-of value))) "widget-class")
                                 (widget-include-bits (class-of value)))) v))))))


(defun get-widget (instance-name &optional group-index)
  (let ((session-name)
        (instance))
    (setf session-name (intern (format nil "~A-~A" (session-value "session-token")
                                       (replace-all (replace-all (car (split-string (request-uri*) #\?)) "/" "-")  "." "-"))))

    (when (gethash session-name *object-cache*)
      (when (gethash instance-name (gethash session-name *object-cache*))
        (when group-index
          (setf instance (gethash group-index (gethash instance-name (gethash session-name *object-cache*)))))
        (unless group-index
          (setf instance (gethash instance-name (gethash session-name *object-cache*))))))
    instance))

(defun set-widget (instance &key group-index )
  "This function instanciates a widget or returns the widget from the dom if it already exists.
Each request uri has its own hashtable with widgets. The hashtable represents a simple dom.
The dom is automatically updated before a request is passed to a hunchentoot handler."
  (let ((session-name))
    (unless (session-value "session-token")
      (setf (session-value "session-token") (random 10810)))
    (setf session-name (intern (format nil "~A-~A" (session-value "session-token")
                                       (replace-all (replace-all (car (split-string (request-uri*) #\?)) "/" "-")  "." "-"))))
    (unless (gethash session-name *object-cache*)
      (setf (gethash session-name *object-cache*) (make-hash-table :test 'equal)))
    (when group-index
      (setf (gethash (instance-name instance) (gethash session-name *object-cache*)) (make-hash-table :test 'equal))
      (setf (gethash group-index (gethash (instance-name instance) (gethash session-name *object-cache*))) instance))
    (unless group-index
      (setf (gethash (instance-name instance) (gethash session-name *object-cache*)) instance))
    instance))
