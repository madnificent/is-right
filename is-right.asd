
(asdf:defsystem :is-right
  :name "common lisp unit test helpers"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0.0.1"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "unit test system which helps the user write tests based on the current implementation of specific functions."
  :depends-on (cl-ppcre closer-mop)
  :serial t
  :components ((:file "packages")
               (:file "is-right")))
