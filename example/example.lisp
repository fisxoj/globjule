(defpackage #:globjule-example
  (:use :cl))

(dolist (i '(globjule cl-glut cl-opengl))
  (require i))

(in-package #:globjule-example)

(defclass globjule-example-window (glut:window)
  ((object :accessor object)
   (vertex-array :accessor vertex-array
		 :initform nil)
   (index-array :accessor index-array))
  (:default-initargs :width 400
		     :height 400
		     :title "Globjule Example"
		     :mode '(:single :Rgb)))

(export 'example)
(defun example ()
  (glut:display-window (make-instance 'globjule-example-window)))

(defmethod glut:display-window :before ((window globjule-example-window))
  (let ((object (globjule:read-file (merge-pathnames "example/example1.obj"
						     (asdf:system-source-directory (asdf:find-system :globjule))))))

    (setf (vertex-array window) (globjule:vertex-array object)
	  (index-array window) (globjule:index-array object)))

  (gl:clear-color 0 0 0 0)
  (gl:shade-model :flat))

(defmethod glut:display ((window globjule-example-window))
  (gl:clear :color-buffer)
  (gl:enable-client-state :vertex-array)
  (gl:load-identity)
  (glu:look-at 0 0 5 0 0 0 0 1 0)
  (gl:color 1 1 1)
  (gl:with-pushed-matrix
    (gl:rotate 30 1 1 0)
    (when (vertex-array window)
      (gl:bind-gl-vertex-array (vertex-array window))
      (gl:draw-elements :triangles (index-array window))))
  (gl:disable-client-state :vertex-array)
  (gl:flush))

(defmethod glut:reshape ((window globjule-example-window) w h)
  (gl:viewport 0 0 w h)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:frustum -1 1 -1 1 1.5 20)
  (gl:matrix-mode :modelview))

(defmethod glut:close ((window globjule-example-window))
  (when (vertex-array window)
    (gl:free-gl-array (vertex-array window)))
  (when (index-array window)
    (gl:free-gl-array (index-array window))))
