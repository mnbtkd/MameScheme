(run-test "test_misc.scm:misc "
          (begin
            [assert (not #t)  #f]
            [assert (not #f)  #t]

            [assert (apply + 1 2)        3] ; malformed ?
            [assert (apply + 1 '(2 3))   6]
            [assert (apply + 1 2 '(3 4 5 6))  21]
            [assert (apply * 2 3 '(4 5))     120]
            [assert (* 3 (apply (lambda(x y)(* x y)) 4 '(5)))  60]

            [assert (map + '(1 2 3) '(10 11 12))  '(11 13 15)]
            [assert (map + '(1 2 3) '(10 11))  '(11 13)]
            [assert (map + '(99) '())  '()]
            [assert (map + '(99))  '(99)]
            [assert (map (lambda(x y)(* x y)) '(1 2 3) '(4 5 6))  '(4 10 18)]

            [assert (let ((v (make-vector 4)))(for-each (lambda(i)(vector-set! v i (* i i))) '(0 1 2 3)) v)  #(0 1 4 9)]

    ;; ARG1(SCH_EOF); CU_ASSERT(subr_is_eof_object(1,1) == SCH_TRUE);
            [assert (eof-object? '())  #f]
            [assert (eof-object? 0)  #f]
            [assert (eof-object? #f)  #f]
            [assert (eof-object? (if #f #f))  #f]

            [assert (let ((p (open-output-file "test.txt")))
               (write-char #\Z p)
               (close-output-port p)
               (let* ((p (open-input-file "test.txt"))
                      (ch (read-char p)))
                 (close-input-port p) ch))  #\Z]
            [assert (let ((p (open-output-file "port_test0.txt")))
               (write-char #\Q p)
               (close-output-port p)
               (call-with-input-file "port_test0.txt" (lambda(p)(read-char p))))  #\Q]

            [assert (begin
              (call-with-output-file "port_test1.txt" (lambda(p)(write-char #\R p)))
              (call-with-input-file  "port_test1.txt" (lambda(p)(read-char p))))  #\R]

            [assert (begin
               (with-output-to-file  "port_test2.txt" (lambda()(write-char #\S)))
               (with-input-from-file "port_test2.txt" (lambda()(read-char))))  #\S]

            ))