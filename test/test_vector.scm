(run-test "test_vector.scm:vector"
          (begin
            [assert (vector-length (make-vector 3))  3]
            [assert (vector-length (make-vector 1))  1]
            [assert (vector? (make-vector 3))  #t]
            [assert (vector? (make-vector 3 4))  #t]
            [assert (vector? 9)  #f]
            [assert (vector? '(a b c))  #f]
            [assert (vector? '())    #f]
            [assert (vector? "abc")  #f]
            [assert (vector? #\z)    #f]
            [assert (let ((v (make-vector 3)))
               (vector-fill! v 66)
               v)  #(66 66 66)]
            [assert (let ((v (make-vector 3)))
               (vector-fill! v 66)
               (vector-ref v 0))  66]
            [assert (let ((v (make-vector 3)))
               (vector-fill! v 66)
               (vector-ref v 1))  66]
            [assert (let ((v (make-vector 3)))
               (vector-fill! v 66)
               (vector-ref v 2))  66]
            [assert (let ((v (make-vector 3)))
               (vector-set! v 0 66)
               (vector-set! v 1 77)
               (vector-set! v 2 88)
               (vector-ref v 0))  66]
            [assert (let ((v (make-vector 3)))
               (vector-set! v 0 66)
               (vector-set! v 1 77)
               (vector-set! v 2 88)
               (vector-ref v 1))  77]
            [assert (let ((v (make-vector 3)))
               (vector-set! v 0 66)
               (vector-set! v 1 77)
               (vector-set! v 2 88)
               (vector-ref v 2))  88]

            [assert (vector->list #(a b c))  '(a b c)]
            [assert (list-ref (vector->list #(1 (6 7 8) 2)) 1)  '(6 7 8)]
            [assert (let (( v (make-vector 3)))
               (vector-fill! v 2) v)  #(2 2 2)]
            [assert (let ((v (vector 1 2 3))) (vector-length v))  3]
            [assert (let ((v (vector 1 2 3))) (vector-ref v 0))   1]
            [assert (let ((v (vector 1 2 3))) (vector-ref v 1))   2]
            [assert (let ((v (vector 1 2 3))) (vector-ref v 2))   3]


            [assert (vector->list #(a b c))  '(a b c)]
            [assert (list-ref (vector->list #(1 (6 7 8) 2)) 1)  '(6 7 8)]
            [assert (let (( v (make-vector 3)))
               (vector-fill! v 2) v)  #(2 2 2)]
            ))
