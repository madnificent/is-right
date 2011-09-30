
(defpackage :is-right
  (:use :common-lisp)
  (:export :is :same :is-right :is-right*
           :test :rm-tests
           :test-all :test-package :test-function
           :failed-test :failed-is-test :failed-same-test
           :mk-package-test-file
           :print-mock-object :print-form :print-subform
           :make-print-form :make-mock-print-form))
