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

;; Texture loading

(defun load-texture (relative-to-pathname filename)
  (let ((filespec (merge-pathnames filename
				   (directory-namestring relative-to-pathname)))
	(texture (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :generate-mipmap t)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)

    (let ((type (string-upcase (pathname-type filespec))))
      (cond
	((string= type "PNG") (load-png filespec))
	((string= type "TGA") (load-tga filespec))
	(t (error "File type ~a is not currently supported" type))))

    (return-from load-texture texture)))

;; These two functions are both only called after the texture has been bound
;; in load-image, so there is no reference to what the texture is being
;; loaded into

(defun load-tga (filespec)
  (let ((image (cl-tga:read-tga filespec)))
    (gl:tex-image-2d :texture-2d 0 :rgba
		     (tga:image-width image) (tga:image-height image)
		     0 (ecase (tga:image-channels image)
			 (3 :bgr)
			 (4 :bgra))
		     :unsigned-byte (tga:image-data image))))


(defun load-png (filespec)
  (let ((image (png-read:read-png-file filespec)))
    (gl:tex-image-2d :texture-2d 0 :rgba
		     (png-read:width image) (png-read:height image)
		     0 :rgba
		     :unsigned-byte (make-array (array-total-size (png-read:image-data image))
						:displaced-to (png-read:image-data image)
						:element-type '(unsigned-byte 8)))))
