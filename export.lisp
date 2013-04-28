(in-package #:globjule)

;;; Preparing OpenGL arrays

(defstruct encoder
  (object)
  (material)
  (used-entries nil)
  (indices nil)
  (n-entries 0)
  (n-indices 0))

(gl:define-gl-array-format vertices
  (gl:vertex :type :float :components (x y z)))

(gl:define-gl-array-format vertex-texture-normal
  (gl:vertex :type :float :components (x y z))
  (gl:tex-coord :type :float :components (u v))
  (gl:normal :type :float :components (nx ny nz)))

(defun make-new-encoder (object)
  (declare (optimize speed space))
  (let ((encoder (make-encoder :object object
			       :used-entries (make-array 0 :adjustable t :fill-pointer 0 :element-type 'cons)
			       :indices (make-array 0 :adjustable t :fill-pointer 0))))

    (dotimes (i (object-n-faces object))
      (dotimes (j 3)
	(let ((face (aref (object-faces object) i)))
	  (declare (type (simple-array integer (3 3)) face))
	  (vector-push-extend (ref-vertex-entry-maybe-create encoder (vector (aref face j 0) (aref face j 1) (aref face j 2)))
			      (encoder-indices encoder))
	  (incf (encoder-n-indices encoder)))))
    encoder))

(defun ref-vertex-entry-maybe-create (encoder v-t-n)
  (declare (optimize speed space))
  (or (find-entry encoder v-t-n)
      (progn
	  ;; Add coordinates to list of used coordinates
	  (vector-push-extend (cons (encoder-n-entries encoder) v-t-n)
			      (encoder-used-entries encoder))
	  (1- (incf (encoder-n-entries encoder))))))

(defun find-entry (encoder v-t-n)
  (declare (optimize speed space)
	   (type (simple-array integer (3)) v-t-n))
  (car (find v-t-n (the vector (encoder-used-entries encoder))
	     :key #'cdr
	     :test (lambda (entry v-t-n)
		     (declare (type (simple-array integer (3)) entry v-t-n))
		     (and (= (aref entry 0)
			     (aref v-t-n 0))
			  (= (aref entry 1)
			     (aref v-t-n 1))
			  (= (aref entry 2)
			     (aref v-t-n 2)))))))

(defun gl-vertex-texture-normal-array (encoder)
  (declare (optimize speed space))
  (declare (type encoder encoder))
  (let ((array (gl:alloc-gl-array 'vertex-texture-normal (encoder-n-entries encoder)))
	(number-of-entries (encoder-n-entries encoder)))

    (dotimes (i number-of-entries)
      (let* ((entry (cdr (aref (encoder-used-entries encoder) i)))
	     (object (encoder-object encoder))
	     (vertex (aref (object-vertices object) (aref entry 0)))
	     (uv (aref (object-texture-vertices object) (aref entry 1)))
	     (normal (aref (object-normals object) (aref entry 2))))
	(setf (gl:glaref array i 'x) (aref vertex 0)
	      (gl:glaref array i 'y) (aref vertex 1)
	      (gl:glaref array i 'z) (aref vertex 2)
	      
	      (gl:glaref array i 'u) (aref uv 0)
	      (gl:glaref array i 'v) (aref uv 1)

	      (gl:glaref array i 'nx) (aref normal 0)
	      (gl:glaref array i 'ny) (aref normal 1)
	      (gl:glaref array i 'nz) (aref normal 2))))

    array))

(defun gl-vertex-array (encoder)
  (declare (type encoder encoder))
  (let ((array (gl:alloc-gl-array 'vertex-texture-normal (encoder-n-entries encoder)))
	(number-of-faces (encoder-n-entries encoder)))

    (dotimes (i number-of-faces)
      (let* ((entry (cdr (aref (encoder-used-entries encoder) i)))
	     (object (encoder-object encoder))
	     (vertex (aref (object-vertices object) (aref entry 0))))
	(setf (gl:glaref array i 'x) (aref vertex 0)
	      (gl:glaref array i 'y) (aref vertex 1)
	      (gl:glaref array i 'z) (aref vertex 2))))
    
    array))

(defun vertex-array (encoder)
  (declare (type encoder encoder))
  (let ((array (make-array (encoder-n-entries encoder) 
			   :element-type '(simple-array single-float (3))
			   :initial-element (make-array 3 
							:element-type 'single-float)))
	(number-of-faces (encoder-n-entries encoder)))

    (dotimes (i number-of-faces)
      (let* ((entry (cdr (aref (encoder-used-entries encoder) i)))
	     (object (encoder-object encoder))
	     (vertex (aref (object-vertices object) (aref entry 0)))
	     (triangle (aref array i)))
	(setf (aref triangle 0) (aref vertex 0)
	      (aref triangle 1) (aref vertex 1)
	      (aref triangle 2) (aref vertex 2))))
    
    array))

(defun gl-index-array (encoder)
  (declare (optimize speed space))
  (let ((array (gl:alloc-gl-array :unsigned-short (encoder-n-indices encoder)))
	(number-of-indices (encoder-n-indices encoder))
	(indices (encoder-indices encoder)))

    (dotimes (i number-of-indices)
      (setf (gl:glaref array i) (aref indices i)))
    array))
