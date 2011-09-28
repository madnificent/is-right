
(cl:defpackage :is-right-tests
  (:use :cl :is-right :asdf))

(cl:in-package :is-right-tests)

(mk-package-test-file :is-right
                      (asdf:system-relative-pathname :is-right "is-right-packagetests.lisp"))
