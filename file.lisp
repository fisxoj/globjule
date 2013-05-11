(in-package #:globjule)

(defstruct object
  (face-vertices 0 :type integer)
  (faces (make-array 0 :adjustable t :fill-pointer 0))
  (n-faces 0 :type integer)
  (shaded nil :type boolean)
  (material nil :type (or null simple-string)))

(defclass file ()
  ((vertices :initform (make-array 0 :adjustable t :fill-pointer 0)
	     :accessor file-vertices)
   (n-vertices :type integer
	       :initform 0
	       :accessor file-n-vertices)
   (texture-vertices :initform (make-array 0 :adjustable t :fill-pointer 0)
		     :accessor file-texture-vertices)
   (n-texture-vertices :type integer
		       :initform 0
		       :accessor file-n-texture-vertices)
   (normals :initform (make-array 0 :adjustable t :fill-pointer 0)
	    :accessor file-normals)
   (n-normals :type integer
	      :initform 0
	      :accessor file-n-normals)
   (objects :type hash-table
	    :initform (make-hash-table :test 'equal)
	    :accessor file-objects)
   (materials :type hash-table
	      :initform (make-hash-table :test 'equal)
	      :accessor file-materials)))

(defun read-file (pathname)
  (declare (optimize speed space))
  (with-open-file (stream pathname)
    (loop
      with file = (make-instance 'file)
      with object of-type (or null object)
      while (peek-char nil stream nil nil)
      do (case (intern (string-upcase (read-string-token stream)) :keyword)
	       
	   (:o
	    (setf object (make-object)
		  (gethash (string-upcase (read-line stream)) (file-objects file))
		  object))
	   (:v 
	    (vector-push-extend (read-vec stream)
				(file-vertices file))
	    (incf (file-n-vertices file)))

	   (:vt 
	    (vector-push-extend (read-vec stream 2)
				(file-texture-vertices file))
	    (incf (file-n-texture-vertices file)))

	   (:vn 
	    (vector-push-extend (read-vec stream)
				(file-normals file))
	    (incf (file-n-normals file)))

	   (:f (let ((vec (make-array '(3 3) :element-type 'integer)))
		 (dotimes (i 3)
		   ;; 1- converts from obj file 1-indexed arrays to
		   ;; 0-indexed arrays so we don't need to think about
		   ;; array indices anymore.
		   (setf (aref vec i 0)
			 (1- (the fixnum (read-from-string (read-until stream #\/))))
			 (aref vec i 1)
			 (1- (the fixnum (read-from-string (read-until stream #\/) nil 1)))
			 (aref vec i 2)
			 (1- (the fixnum (read-from-string (read-until stream #\Space) nil 1)))))
		 (vector-push-extend vec (object-faces object))
		 (incf (object-n-faces object))))
	   (:s (setf (object-shaded object) (case (read-line stream nil nil)
					      (1 t)
					      (0 nil))))
	   (:usemtl
	    (setf (object-material object) (let ((material (read-line stream nil nil)))
					     (unless (string= material "None")
					       material))))
	   ;; Make a new material for use
	   (:mtllib
	    
	    (read-mtl (merge-pathnames (read-line stream nil nil)
				       (directory-namestring pathname))
		      (file-materials file)))
	   (t (read-line stream nil nil)))
      finally (return file))))

(defun get-object (file object)
  (gethash (string-upcase object) (file-objects file)))

(defun get-material (file material)
  (gethash (string-upcase material) (file-materials file)))
