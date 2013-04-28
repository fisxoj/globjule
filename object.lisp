(in-package #:globjule)

(defstruct object
  (vertices (make-array 0 :adjustable t :fill-pointer 0))
  (n-vertices 0)
  (texture-vertices (make-array 0 :adjustable t :fill-pointer 0))
  (n-texture-vertices 0)
  (normals (make-array 0 :adjustable t :fill-pointer 0))
  (n-normals 0)
  (face-vertices 0)
  (faces (make-array 0 :adjustable t :fill-pointer 0))
  (n-faces 0)
  (shaded nil)
  (material nil))

(defun read-object (stream)
  (loop
    with object = (make-object)
    while (peek-char nil stream nil nil)
    do (case (intern (string-upcase (read-string-token stream)) :keyword)
	 (:v 
	  (vector-push-extend (read-vec stream)
			      (object-vertices object))
	  (incf (object-n-vertices object)))

	 (:vt 
	  (vector-push-extend (read-vec stream 2)
			      (object-texture-vertices object))
	  (incf (object-n-texture-vertices object)))

	 (:vn 
	  (vector-push-extend (read-vec stream)
			      (object-normals object))
	  (incf (object-n-normals object)))

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
	 (:s (case (read-line stream nil nil)
	       (1 t)
	       (0 nil)))
	 (:usemtl
	  (setf (object-material object) (let ((material (read-line stream nil nil)))
					   (unless (string= material "None")
					     material))))
	 (t (read-line stream nil nil)))
    finally (return object)))
