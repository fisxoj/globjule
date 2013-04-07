;;;; globjule.asd

(asdf:defsystem #:globjule
  :serial t
  :description "Describe globjule here"
  :author "Matt Novenstern <fisxoj@gmail.com>"
  :license "MIT"
  :components ((:file "package")
               (:file "globjule"))
  :depends-on (#:cl-opengl))

