(in-package is-right)
(rm-tests (common-lisp:find-package ':is-right))

(test 'is-right::make-test-for-function-execution
      (is-right::same '(test '+ (is-right::same 3 (+ 1 2)))
                      (is-right::make-test-for-function-execution '+ '(+ 1 2))))

(test 'is-right::make-test-for-function-execution
      (is-right::same
       '(test 'loop
              (is-right::same 1
                              (loop for x from 0 below 2
                                    sum x)))
       (is-right::make-test-for-function-execution 'loop
                                                   '(loop for x from 0 below 2
                                                          sum x))))

(test 'is-right::make-get-right-test
      (is-right::same '(test '+ (is-right::same 6 (+ 1 2 3)))
                      (is-right::make-get-right-test '(+ 1 2 3))))

(test 'is-right::make-get-right-test
      (is-right::same
       '(test '+
              (is-right::same 6
                              (let ((x 1))
                                (+ x 2 3))))
       (is-right::make-get-right-test '+
                                      '(let ((x 1))
                                         (+ x 2 3)))))

(test 'is-right::make-get-right*-test
      (is-right::same
       '(test '+
              '(let ((x 1))
                 (is-right::same 6 (+ x 2 3))))
       (is-right::make-get-right*-test
        '(let ((x 1))
           (is (+ x 2 3))))))

(test 'is-right::make-get-right*-test
      (is-right::same
       '(test 'apply
              '(let ((x 10))
                 (is-right::same 45
                                 (apply #'+
                                        (loop for y from 0 below x
                                              collect y)))))
       (is-right::make-get-right*-test
        '(let ((x 10))
           (is
            (apply #'+
                   (loop for y from 0 below x
                         collect y)))))))

(test 'is-right::make-get-right*-test
      (is-right::same
       '(test 'apply
              '(let ((x 4))
                 (is-right::same 6
                                 (apply #'+
                                        (loop for y from 0 below x
                                              collect y)))))
       (is-right::make-get-right*-test
        '(let ((x 4))
           (is
            (apply #'+
                   (loop for y from 0 below x
                         collect y)))))))
(test 'is-right::make-print-form
      (same 't
            (let ((object '(1 2 3 4)))
              (equal object (eval (make-print-form object))))))

(test 'is-right::make-print-form
      (same 't
            (let ((object '(foo bar baz)))
              (equal object (eval (make-print-form object))))))

