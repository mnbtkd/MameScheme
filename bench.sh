foo=0
while [ "$foo" -lt 10 ]
do
  (time ./mame -e "(begin (define fib (lambda(n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))) (fib 33))") 2> ./prof/prof_fib_$foo
  (time ./mame -e "(begin (define tak (lambda(x y z) (if (not (< y x)) z (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))))) (tak 18 9 0))") 2> ./prof/prof_tak_$foo
  (time ./mame -e "(p1 '(let ((a (if #t 9 10)))(if (< a 10)(* a a)(* a a a))))") 2> ./prof/prof_p1_$foo
  foo=$(($foo+1))
done


/usr/local/bin/gosh bench.scm 10

exit 0

#(define (ack m n)
#  (cond ((= m 0) (+ n 1))
#        ((= n 0) (ack (- m 1) 1))
#        (else (ack (- m 1) (ack m (- n 1))))))
