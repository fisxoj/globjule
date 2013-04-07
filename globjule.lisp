;;;; globjule.lisp

(in-package #:globjule)

(defstruct object
  (vertices (make-array 0 :adjustable t :fill-pointer 0))
  (texture-vertices (make-array 0 :adjustable t :fill-pointer 0))
  (normals (make-array 0 :adjustable t :fill-pointer 0))
  (faces (make-array 0 :adjustable t :fill-pointer 0)))

(defun make-vec (n)
  (make-array n :element-type 'single-float))


;;; File reading

(defun read-string-token (stream)
  (read-until stream #\Space))

(defun read-token (stream)
  (read (read-string-token stream)))

(defun read-until (stream character)
  (when (typep stream 'string)
    (setf stream (make-string-input-stream stream)))
  (loop with head
	for c = (read-char stream nil nil)
	if (or (not c) (char= c character) (char= c #\Newline))
	  do (return (coerce (reverse head) 'string))
	else
	  do (push c head)))

(defun read-file (pathname)
  (with-open-file (stream pathname)
    (loop
	  with object = (make-object)
	  while (peek-char nil stream nil nil)
	  do (case (intern (string-upcase (read-string-token stream)) :keyword)
	       (:v (let ((vec (make-vec 3)))
		     (setf (aref vec 0) (read stream)
			   (aref vec 1) (read stream)
			   (aref vec 2) (read stream))
		     (vector-push-extend vec (object-vertices object))))

	       (:vt (let ((vec (make-vec 2)))
		     (setf (aref vec 0) (read stream)
			   (aref vec 1) (read stream))
		     (vector-push-extend vec (object-texture-vertices object))))

	       (:vn (let ((vec (make-vec 3)))
		     (setf (aref vec 0) (read stream)
			   (aref vec 1) (read stream)
			   (aref vec 2) (read stream))
		     (vector-push-extend vec (object-normals object))))

	       (:f (let ((vec (make-array '(3 3) :element-type 'integer)))
			 (dotimes (i 3)
			   ;; 1- converts from obj file 1-indexed arrays to
			   ;; 0-indexed arrays so we don't need to think about
			   ;; array indices anymore.
			   (setf (aref vec i 0)
				 (1- (read-from-string (read-until stream #\/)))
				 (aref vec i 1)
				 (1- (read-from-string (read-until stream #\/)))
				 (aref vec i 2)
				 (1- (read-from-string (read-until stream #\Space)))))
			 (vector-push-extend vec (object-faces object))))
	       (:s (warn "s not implemented") (read-line stream nil nil))
	       (:usemtl (warn "usemtl not implemented") (read-line stream nil nil))
	       (:mtllib (warn "mtllib not implemented") (read-line stream nil nil))
	       (t (read-line stream nil nil)))
	  finally (return object))))


;;; Preparing OpenGL arrays

(gl:define-gl-array-format vertices
  (gl:vertex :type :float :components (x y z)))

(defun vertex-array (object)
  (let ((array (gl:alloc-gl-array 'vertices (length (object-vertices object))))
	(number-of-vertices (length (object-vertices object)))
	(vertices (object-vertices object)))
    (dotimes (i number-of-vertices)
      (setf (gl:glaref array i 'x) (aref (aref vertices i) 0)
	    (gl:glaref array i 'y) (aref (aref vertices i) 1)
	    (gl:glaref array i 'z) (aref (aref vertices i) 2)))
    array))

(defun index-array (object)
  (let ((array (gl:alloc-gl-array :unsigned-short (* 3 (length (object-vertices object)))))
	(number-of-vertices (length (object-vertices object)))
	(indices (object-faces object)))
    (dotimes (i number-of-vertices)
      (setf (gl:glaref array (* i 3)) (aref (aref indices i) 0 0)
	    (gl:glaref array (+ (* i 3) 1)) (aref (aref indices i) 1 0)
	    (gl:glaref array (+ (* i 3) 2)) (aref (aref indices i) 2 0)))
    array))
