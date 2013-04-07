;;;; globjule.asd

(asdf:defsystem #:globjule
  :serial t
  :description "Library for loading 3d data from .obj files.  Can export them to OpenGL-friendly formats."
  :author "Matt Novenstern <fisxoj@gmail.com>"
  :license "MIT"
  :components ((:file "package")
               (:file "globjule"))
  :depends-on (#:cl-opengl))

