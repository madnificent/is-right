
(in-package :is-right)


(defvar *unit-tests* nil
  "contains a plist of packages.  the contents of the plist is a plist of functions.  the plist of functions has a regular lisp list as values, each containing a test.")

(defun distill-function-information (function-symbol)
  "tries to distill the package and the symbol representing the function from <function-symbol> this is only guaranteed to work when function-symbol is a symbol, yet we try to solve it non-portably, when function-symbol is a function as well.

   unless an error is signaled, this function returns two values.  both of the values are symbols.  the first value is a symbol representing the function, the second value is a symbol representing the package."
  (setf function-symbol
        (cond ((functionp function-symbol)
               (let ((symbol
                      (nth-value 2 (function-lambda-expression function-symbol))))
                 (when (or (null symbol)
                           (not (symbolp symbol)))
                   (error "Test based on function ~A for which we can't find a related symbol." function-symbol))
                 symbol))
              ((symbolp function-symbol)
               function-symbol)
              (T (error "Test based on function ~A, which can't be translated to a related symbol." function-symbol))))
  (values function-symbol
          (symbol-package function-symbol)))

(defun test (function test-form)
  "test-function is the function version to add tests to the is-right test framework.
   function is expected to be the function which will be tested.  only the symbol of th function is supported portably.
   test-form is the form which will be executed as a test for the given function."
  (multiple-value-bind (function-symbol package)
      (distill-function-information function)
    (push test-form
          (getf (getf *unit-tests* package)
                function-symbol))))

(defun get-tests-for-function (function)
  "returns a list of all tests which belong to function."
  (multiple-value-bind (symbol package)
      (distill-function-information function)
    (getf (getf *unit-tests* package)
          symbol)))

(defun get-tests-for-package (package)
  "returns the tests for the given package."
  (when (symbolp package)
    (setf package (find-package package)))
  (getf *unit-tests* package))

(defun rm-tests (function-symbol)
  "removes the tests for the given function"
  (multiple-value-bind (function-symbol package)
      (distill-function-information function-symbol)
    (setf (getf (getf *unit-tests* package)
                function-symbol)
          nil)))

(define-condition failed-test (error)
  ((test :initarg :test
         :reader test-form)
   (explenation :initarg :explenation
                :reader explenation)
   (complete-test :initarg :complete-test
                  :reader complete-test))
  (:documentation "error which is thrown when a test fails to execute"))

(define-condition failed-is-test (failed-test)
  ((explenation :initform "a form containing 'is failed to return a non-nil value."))
  (:documentation "error which is thrown when an is-test failed to return a non-nil value."))

(defmethod print-object ((object failed-test) stream)
  (print-unreadable-object (object stream)
    (format stream "~& Explenation: ~A~& Test: ~A~& Complete test: ~A~&" (explenation object) (test-form object) (complete-test object))))

(defvar *complete-test* nil
  "contains the complete form which is currently being tested")

(defmacro is (form)
  "verifies that form returns a non-nil value."
  `(unless ,form
     (error 'failed-is-test
            :test (quote ,form)
            :complete-test *complete-test*)))

(defun execute-test (test-form)
  "executes a single test"
  (let ((*complete-test* test-form))
    (eval test-form)))

(defun test-function* (symbol &rest tests)
  "runs all tests in <tests> for the function denoted by symbol <symbol>"
  (format T "~&~A (~A) ~t" symbol (length tests))
  (force-output)
  (dolist (test tests)
    (restart-case (progn
                    (execute-test test)
                    (format T "."))
      (accept-test-failure ()
        (format T "X")))
    (force-output)))

(defun test-function (function)
  "tests the given function"
  (let ((function-symbol (distill-function-information function)))
    (apply #'test-function*
           function-symbol
           (get-tests-for-function function-symbol))))

(defun test-package* (package &rest function-plist)
  "tests all the given functions in the plist <function-plist> and reports output for the tests, given that they come from <package>."
  (format T "~&Running tests in ~A (~A)~&" package
          (loop for (name tests . rest) on function-plist by #'cddr
             sum (length tests)))
  (format T "~&------------------------------~&")
  (loop for (name tests . rest) on function-plist by #'cddr
     do (apply #'test-function* name tests)))

(defun test-package (package)
  "tests each function in <package> and lists the execution."
  (unless (packagep package)
    (setf package (find-package package)))
  (apply #'test-package*
         package
         (get-tests-for-package package)))

(defun test-all ()
  "runs the tests for each monitored package"
  (format T "~&TESTING ALL PACKAGES (~A)~&"
          (loop for (package fplist . rest) on *unit-tests* by #'cddr
             sum (loop for (function tests . rest) on fplist by #'cddr
                    sum (length tests))))
  (format T "~&==============================~&")
  (loop for (package fplist . rest) on *unit-tests* by #'cddr
     do (progn
          (format T "~&~%")
          (apply #'test-package* package fplist)
          (format T "~&~%==============================~&"))))
