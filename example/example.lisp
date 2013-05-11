(defpackage #:globjule-example
  (:use :cl))

(dolist (i '(globjule cl-glu cl-glut cl-opengl))
  (require i))

(in-package #:globjule-example)

(defclass globjule-example-window (glut:window)
  ((object :accessor object)
   (glob :accessor window-glob
	 :initform nil))
  (:default-initargs :width 400
		     :height 400
		     :title "Globjule Example"
		     :mode '(:single :rgb)))

(export 'example)
(defun example ()
  (glut:display-window (make-instance 'globjule-example-window)))

(defmethod glut:display-window :before ((window globjule-example-window))
  (let ((glob (globjule:load-globjule
	       (globjule:read-file (merge-pathnames "example/example1.obj"
						    (asdf:system-source-directory (asdf:find-system :globjule))))
	       "example1")))

    (setf (window-glob window) glob)

    (gl:clear-color 0 0 0 0)
    (gl:shade-model (if (globjule:globjule-shaded glob)
			:shaded
			:flat))))

(defmethod glut:display ((window globjule-example-window))
  (gl:clear :color-buffer)
  (gl:enable-client-state :vertex-array)
  (gl:load-identity)
  (glu:look-at 0 0 5 0 0 0 0 1 0)
  (gl:color 1 1 1)
  (gl:with-pushed-matrix
    (gl:rotate 30 1 1 0)
    (let ((glob (window-glob window)))
      (when glob
	(gl:bind-gl-vertex-array (globjule:globjule-vertex-array glob))
	(gl:draw-elements :triangles (globjule:globjule-index-array glob)))))
  (gl:disable-client-state :vertex-array)
  (gl:flush))

(defmethod glut:reshape ((window globjule-example-window) w h)
  (gl:viewport 0 0 w h)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:frustum -1 1 -1 1 1.5 20)
  (gl:matrix-mode :modelview))

(defmethod glut:close ((window globjule-example-window))
  (let ((glob (window-glob window)))
    (when glob
      (when (globjule:globjule-vertex-array glob)
	(gl:free-gl-array (globjule:globjule-vertex-array glob)))
      (when (globjule:globjule-index-array glob)
	(gl:free-gl-array (globjule:globjule-index-array glob))))))

(example)
