(in-package #:globjule)

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

(defun read-vec (stream &optional (n 3))
  (let ((vec (make-vec n)))
    (declare (type (simple-array single-float (*)) vec))
    (dotimes (i n)
      (setf (aref vec i) (read stream)))
    vec))

(defun load-image (relative-to-pathname filename)
  (let ((image (tga:read-tga (merge-pathnames filename
					      (directory-namestring relative-to-pathname))))
	(texture (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

    (gl:tex-image-2d :texture-2d 0 :rgba
		     (tga:image-width image) (tga:image-height image)
		     0 (ecase (tga:image-channels image)
			 (3 :bgr)
			 (4 :bgra))
		     :unsigned-byte (tga:image-data image))
    (return-from load-image texture)))
