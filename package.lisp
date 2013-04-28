;;;; package.lisp

(defpackage #:globjule
  (:use #:cl)
  (:export #:read-file
	   #:load-globjule
	   #:gl-index-array
	   #:gl-vertex-array
	   #:gl-vertex-texture-normal-array
	   #:veretx-array

	   #:globjule-index-array
	   #:globjule-vertex-array
	   #:globjule-shaded
	   #:globjule-ns
	   #:globjule-ka
	   #:globjule-kd
	   #:globjule-ks
	   #:globjule-ni
	   #:globjule-map-kd))

