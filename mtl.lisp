(in-package #:globjule)

(defstruct material
  ns
  ka
  kd
  ks
  ni
  d 
  illum
  map-ka
  map-kd 
  map-ks)

(defun read-mtl (pathname)
  (with-open-file (stream pathname)
    (loop with material = (make-material)
	  with name of-type string
	  while (peek-char nil stream nil nil)
	  do (case (intern (string-upcase (read-string-token stream)) :keyword)
	       (:newmtl
		(setf name
		      (read-line stream)))
	       (:ns
		(setf (material-ns material)
		      (read-line stream)))
	       (:ka
		(setf (material-ka material)
		      (read-vec stream)))
	       (:kd
		(setf (material-kd material)
		      (read-vec stream)))
	       (:ks
		(setf (material-ks material)
		      (read-vec stream)))
	       (:ni
		(setf (material-ni material)
		      (read-line stream)))
	       (:d
		(setf (material-d material)
		      (read-line stream)))
	       (:illum
		(setf (material-illum material) 
		      (case (read-line stream)
			(0 :none)
			(1 :no-specular)
			(2 :full))))
	       (:map_ka
		(setf (material-map-ka material)
		      (load-image pathname (read-line))))
	       (:map_kd
		(setf (material-map-kd material)
		      (load-image pathname (read-line))))
	       (:map_ks
		(setf (material-map-ka material)
		      (load-image pathname (read-line)))))
	  finally (return (values name material)))))
