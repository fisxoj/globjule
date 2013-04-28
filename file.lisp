(in-package #:globjule)

(defclass file ()
  ((objects :type hash-table
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
	  while (peek-char nil stream nil nil)
	  do (case (intern (string-upcase (read-string-token stream)) :keyword)
	       
	       (:o
		(setf (gethash (string-upcase (read-line stream)) (file-objects file))
		      (read-object stream)))
	       (:mtllib 
		(multiple-value-bind (name mtl) 
		    (read-mtl (merge-pathnames (read-line stream nil nil)
					       (directory-namestring pathname)))
		  (setf (gethash (string-upcase name) (file-materials file)) mtl)))
	       (t (read-line stream nil nil)))
	  finally (return file))))

(defun get-object (file object)
  (gethash (string-upcase object) (file-objects file)))

(defun get-material (file material)
  (gethash (string-upcase material) (file-objects file)))
