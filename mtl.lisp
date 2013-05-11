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

(defun read-mtl (pathname materials-hash-table)
  (declare (optimize space speed))
  (with-open-file (stream pathname)
    (loop with material = (make-material)
	  while (peek-char nil stream nil nil)
	  do (case (intern (string-upcase (read-string-token stream)) :keyword)
	       
	       (:|#| (read-line stream))
	       (:newmtl
		(setf material (make-material)
		      (gethash (string-upcase (read-line stream)) materials-hash-table)
		      material))
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
		      (load-texture pathname (read-line stream))))
	       (:map_kd
		(setf (material-map-kd material)
		      (load-texture pathname (read-line stream))))
	       (:map_ks
		(setf (material-map-ka material)
		      (load-texture pathname (read-line stream))))
	       (t 
		;; FIXME: Currently catches spurious newlines, should probbly fix
		;; this by using the read functions better
		)))))
