;;;; globjule.lisp

(in-package #:globjule)

(defclass globjule ()
  ((vertex-array :accessor globjule-vertex-array
		 :initarg :vertex-array)
   (index-array :accessor globjule-index-array
		:initarg :index-array)
   (shaded :type boolean
	   :accessor globjule-shaded
	   :initarg :shaded)
   (ns :type single-float
       :accessor globjule-ns
       :initarg :ns)
   (ka :type (simple-array single-float (3))
       :accessor globjule-ka
       :initarg :ka)
   (kd :type (simple-array single-float (3))
       :accessor globjule-kd
       :initarg :kd)
   (ks :type (simple-array single-float (3))
       :accessor globjule-ks
       :initarg :ks)
   (ni :type single-float
       :accessor globjule-ni
       :initarg :ni)
   (map-kd :type integer
	   :accessor globjule-map-kd
	   :initarg :map-kd)))

(defun load-globjule-from-file (filespec object)
  (let ((file (read-file filespec)))
    (load-globjule file object)))

(defun load-globjule (file obj)
  (let* ((object   (get-object file obj))
	 (encoder  (make-new-encoder object))
	 (material (get-material file (object-material object)))
	 (glob     (make-instance 'globjule
				  :vertex-array (gl-vertex-texture-normal-array encoder)
				  :index-array (gl-index-array encoder)
				  :shaded (object-shaded object))))
    (when material
      (with-slots (ns ka kd ks ni map-kd) material
	(setf ns     (material-ns material)
	      ka     (material-ka material)
	      kd     (material-kd material)
	      ks     (material-ks material)
	      ni     (material-ni material)
	      map-kd (material-map-kd material))))
    glob))

(defun object-vertex-array (file object)
  (let* ((object (get-object file object))
	 (encoder (make-new-encoder object)))
    (vertex-array encoder)))
