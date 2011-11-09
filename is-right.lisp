
(in-package :is-right)


(defvar *unit-tests* nil
  "contains a plist of packages.  the contents of the plist is a plist of functions.  the plist of functions has a regular lisp list as values, each containing a test.")

(defun distill-function-information (function-symbol)
  "tries to distill the package and the symbol representing the function from <function-symbol> this is only guaranteed to work when function-symbol is a symbol, yet we try to solve it non-portably, when function-symbol is a function as well.

   unless an error is signaled, this function returns two values.  the first value is a symbol representing the function, the second value is the package of the symbol."
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
   function is expected to be the function which will be tested.  only the symbol of the function is supported portably.
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

(defun rm-tests (function-or-package)
  "removes the tests for the given package or function"
  (if (symbolp function-or-package)
      (multiple-value-bind (function-symbol package)
          (distill-function-information function-or-package)
        (setf (getf (getf *unit-tests* package)
                    function-symbol)
              nil))
      (setf (getf *unit-tests* function-or-package) nil)))

(define-condition failed-test (error)
  ((test :initarg :test
         :reader test-form)
   (explenation :initarg :explenation
                :reader explenation)
   (complete-test :initarg :complete-test
                  :reader complete-test))
  (:documentation "error which is thrown when a test fails to execute"))

(defmethod print-object ((object failed-test) stream)
  (print-unreadable-object (object stream)
    (format stream "~& Explenation: ~A~& Test: ~A~& Complete test: ~A~&" (explenation object) (test-form object) (complete-test object))))

(define-condition failed-is-test (failed-test)
  ((explenation :initform "a form containing 'is failed to return a non-nil value."))
  (:documentation "error which is thrown when an is-test failed to return a non-nil value."))

(defvar *complete-test* nil
  "contains the complete form which is currently being tested")

(defmacro is (form)
  "verifies that form returns a non-nil value.

   the implementation of this macro is shadowed in is-right*"
  `(unless ,form
     (error 'failed-is-test
            :test (quote ,form)
            :complete-test *complete-test*)))

(defun execute-test (test-form)
  "executes a single test"
  (let ((*complete-test* test-form))
    (eval test-form)))
  
(define-condition failed-same-test (failed-test)
  ((explenation :initform "a form containing 'same failed to return both the same values.  check received value and expected value for the resulting forms.")
   (received-value :initarg :received
                   :reader received-value)
   (expected-value :initarg :expected
                   :reader expected-value)))

(defmethod print-object ((err failed-same-test) stream)
  (print-unreadable-object (err stream)
    (format stream "~& Explenation: ~A~& Test: ~A~& Complete test: ~A~& Received: ~A~& Expected: ~A~&" (explenation err) (test-form err) (complete-test err) (received-value err) (expected-value err))))
  
(defmacro same (expected-value form)
  "verifies that form returns a value which appears to be equal to the expected value."
  (let ((g-returned-value (gensym))
        (g-expected-value (gensym)))
    `(let ((,g-returned-value ,form)
           (,g-expected-value ,expected-value))
       (unless (equal ,g-returned-value ,g-expected-value)
         (error 'failed-same-test
                :test (quote ,form)
                :complete-test *complete-test*
                :expected ,g-expected-value
                :received ,g-returned-value))
       ,g-returned-value)))

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



(defgeneric print-subform (object)
  (:documentation "when you use this system and you need to print an instance which isn't directly related to yourself, you should call the print-subform method on that object.  this will ensure that the correct order of execution is used."))

(defgeneric print-mock-object (function object)
  (:documentation "prints a mock object for the given function.  this allows you to load only a specific aspect of the object for the given function."))

(defgeneric print-form (object)
  (:documentation "adds an s-expression, which, when evaluated, will yield an equivalent object."))
  
(defclass base-printlevel ()
  ()
  (:documentation "base class for the printing of instances."))

(defclass form-printlevel (base-printlevel)
  ()
  (:documentation "prints the objects so reading/evaluating them will result in an equivalent object."))

(defclass mock-printlevel (form-printlevel)
  ((test-function-symbol :initform nil
                         :initarg :test-function-symbol
                         :reader test-function-symbol))
  (:documentation "prints the instance so that reading/evaluating them will result in an object which has all the needed similarities for the given test."))

(defparameter *current-printlevel* nil
  "variable which is shadowed when printing occurs, so print-subform can call the correct toplevel function.")

(define-condition unsupported-printlevel (error)
  ()
  (:documentation "indicates that the printlevel was not supported for the given objects"))

(defmethod print-mock-object (function object)
  (error 'unsupported-printlevel))
(defmethod print-form (object)
  (error 'unsupported-printlevel))

(defgeneric call-printlevel (printlevel object)
  (:documentation "prints the object in the given printlevel"))

(defmethod call-printlevel ((printlevel base-printlevel) object)
  object)

(defmethod call-printlevel :around ((printlevel form-printlevel) object)
  (handler-case
      (print-form object)
    (unsupported-printlevel ()
      (call-next-method))))

(defmethod call-printlevel :around ((printlevel mock-printlevel) object)
  (handler-case
      (print-mock-object (test-function-symbol printlevel)
                         object)
    (unsupported-printlevel ()
      (call-next-method))))

(defun boot-printing (printlevel object)
  "boots the printing of any currently known printlevel.  the printlevel-specific functions use this."
  (let ((*current-printlevel* printlevel))
    (call-printlevel printlevel object)))

(defun make-print-form (object)
  "prints the object as a form through the print-form generic function."
  (boot-printing (make-instance 'form-printlevel)
                 object))

(defun make-mock-print-form (object function)
  "prints the object as a mock object through the print-mock-object generic function"
  (boot-printing (make-instance 'mock-printlevel
                                :test-function-symbol function)
                 object))

(defmethod print-subform (object)
  (boot-printing *current-printlevel* object))

(defun pure-list-p (cons-cell)
  (when (listp cons-cell)
    (loop for (a . b) on cons-cell
       unless (listp b) return nil
       collect a)))

(defmethod print-form ((cell cons))
  (if (pure-list-p cell)
      (cons 'list (loop for item in cell collect (print-subform item)))
      `(cons ,(print-subform (car cell))
             ,(print-subform (cdr cell)))))

(defmethod print-form ((seq sequence))
  ;; TODO: make this correct, you want everything of the normal array, but not the element-type
  (let ((new-array
         (make-array (array-dimensions seq))))
    (loop for i from 0 below (length seq)
       do (setf (elt new-array i)
                (print-subform (elt seq i))))
    `(quote ,new-array)))

(defmethod print-form ((string string))
  string)

(defmethod print-form ((symbol symbol))
  `(quote ,symbol))

(defun make-test-for-function-execution (function form)
  "creates a test for the function execution of form"
  (let ((execution-value (eval form)))
    `(test ',function '(same ,(make-mock-print-form execution-value function) ,form))))

(defun make-get-right-test (form &optional form-when-test-overridden)
  "returns a test-form for the test which can be constructed from the current execution of form"
  (let* ((function-form (if form-when-test-overridden
                            form-when-test-overridden
                            form))
         (function-symbol (if form-when-test-overridden
                              form
                              (first function-form))))
    (values (make-test-for-function-execution function-symbol function-form)
            function-symbol)))

(defun make-get-right*-test (form &optional form-when-test-overridden)
  (let* ((function-form (if form-when-test-overridden
                            form-when-test-overridden
                            form))
         (function-symbol (when form-when-test-overridden
                            form)))
    (let ((execution-values nil))
      (labels ((get-function-values (form)
                 (if (listp form)
                     (if (eq (first form) 'is)
                         `(push (make-mock-print-form
                                 ,(second form)
                                 ',(or function-symbol
                                       (if (listp (second form))
                                           (first (second form))
                                           (second form))))
                                execution-values)
                         (loop for expression in form
                            collect (get-function-values expression)))
                     form)))
        (setf execution-values
              (reverse (eval `(let ((execution-values nil))
                                ,(get-function-values function-form)
                                execution-values))))
        (labels ((walk-get-right-form (form)
                   (if (eq (first form) 'is)
                       ;; translate if-form
                       (let ((function-form (second form)))
                         (setf function-symbol
                               (or function-symbol (first function-form)))
                         `(same ,(pop execution-values) ,function-form))
                       ;; walk other forms
                       (loop for expression in form
                          collect (if (listp expression)
                                      (walk-get-right-form expression)
                                      expression)))))
          (let ((new-form (walk-get-right-form function-form)))
            (values `(test ',function-symbol
                           (quote ,new-form))
                    function-symbol)))))))

(defvar *package-files* nil
  "a plist which contains the package as key and for each package for which the location where the tests need to be stored is known, the path to that file.")

(defun packagetests-file (package)
  "setfable place for the file in which the package tests are defined, or nil if such a file is not known."
  (unless (packagep package)
    (setf package (find-package package)))
  (getf *package-files* package))

(defun (setf packagetests-file) (file package)
  (unless (packagep package)
    (setf package (find-package package)))
  (setf (getf *package-files* package)
        file))

(defun ensure-packagetest-file (file package-symbol)
  "checks whether or not file exists. if the file doesn't exist an initial declaration is added which will clear the known tests from the current system when interpreted."
  (setf (packagetests-file package-symbol) file)
  (unless (probe-file file)
    (with-open-file (out file :direction :output)
      (let ((*package* (find-package package-symbol)))
        (write `(cl:in-package ,package-symbol) :stream out :readably t)
        (write `(is-right:rm-tests (cl:find-package ',package-symbol)) :stream out :readably t)
        (format out "~&~%")))))

(defun add-test-to-file (function form)
  "adds <form>, which is a complete test for function <function>, to the files which are known."
  (let ((package (nth-value 1 (distill-function-information function))))
    (with-open-file (out (packagetests-file package)
                        :direction :output
                        :if-exists :append)
      (let ((*package* package))
        (write form :stream out :readably t)
        (format out "~&~%")))))

(defun mk-package-test-file (package-symbol file)
  "ensures that a package test file for package-symbol exists and is located in file.  furthermore ensures that the system knows the file for further additions.  it also reads the contents of the file, so the tests can be interpreted."
  (ensure-packagetest-file file package-symbol)
  (setf (packagetests-file package-symbol) file)
  (load file))

(defmacro is-right (form &optional form-when-symbol-overridden)
  "adds the test for form to the set of tests for the given function"
  (multiple-value-bind (test func)
      (make-get-right-test form form-when-symbol-overridden)
    (add-test-to-file func test)
    test))

(defmacro is-right* (form &optional form-when-symbol-overridden)
  (multiple-value-bind (test func)
      (make-get-right*-test form form-when-symbol-overridden)
    (add-test-to-file func test)
    test))
