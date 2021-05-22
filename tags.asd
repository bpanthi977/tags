;;;; tags.asd

(asdf:defsystem #:tags
  :description "Describe tags here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop #:osicat)
  :components ((:file "package")
               (:file "tags")))

