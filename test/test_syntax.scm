(run-test "test_syntax.scm:syntax "
          (begin
            [assert (quote a)  'a]
            [assert (+ 4 5)  9]
            [assert (* 2 3)  6]
            [assert (- 9 3)  6]
            [assert (/ 8 2)  4]
            [assert (/ (* 3 (+ 4 5)) (/ 6 (- 5 3)))  9]
            [assert (begin (+ 1 2)(* 2 3)(/ 6 2))  3]
            [assert (if #t (+ 3 4)(+ 5 6))  7]
            [assert (if #f (+ 3 4)(+ 5 6))  11]
            [assert ((lambda(x)(* x x)) 9)  81]
            [assert ((lambda(x)(begin (set! x 88) x )) 77)  88]
            [assert ((((lambda(x)(lambda(y)(lambda(z)(* z z)))) 11) 22) 33)  1089]
            [assert ((lambda(a b c) (+ a b) (+ b c) (+ c a)) 1 2 3)  4]

            [assert ((lambda(x y . z) z) 1 2 3 4)  '(3 4)]
            [assert ((lambda x (car x)) '(a b c))  '(a b c)]

            [assert (begin (define x 99) x)  99]
            [assert (begin (define x 99)(define x 77)(define y 66)(define y 55)(set! x 88)(set! y 77) x)  88]
            [assert (begin (define a 9)(set! a 6)(* a a))            36]
            [assert (begin (define x 99)(set! x 88) x)  88]
            [assert (begin (define fn1 (lambda(x)(+ x x))) (fn1 6))  12]
            [assert (begin (define x 99)(define fn1 (lambda()(+ x x))) (fn1))  198]
            [assert (if (begin (define x 99) #t) (begin (set! x 88) x) (begin (set! x 77) x))  88]
            [assert (if (begin (define x 99) #f) (begin (set! x 88) x) (begin (set! x 77) x))  77]
            [assert (begin (define fn (lambda(n)(if (< 0 n)(+ n (fn (- n 1))) n))) (fn 3))  6]
            [assert (begin (define fact (lambda(n) (if (> 1 n) 1 (* n (fact (- n 1)))))) (fact 10))  3628800]
            [assert (begin (define fact (lambda (n a) (if (> 1 n) a (fact (- n 1) (* a n ))))) (fact 10 1))  3628800]
            [assert (begin (define fib (lambda (x) (if (< x 2) 1 (+ (fib (- x 1)) (fib (- x 2)))))) (fib 28))  514229]
            [assert (if (or (< 2 1)(< 1 2)) 9 8)   9]
            [assert (if (and (< 1 2)(< 2 1)) 9 8)  8]
            [assert (if (and (< 1 2)(< 3 4)) 9 8)  9]

            [assert (let ((a 1) (b 2)) (+ a b))  3]
            [assert (let ((fn (lambda(x)(* x x)))) (fn 10))  100]
            [assert (letrec ((even? (lambda (n) (if (zero? n) #t (odd? (- n 1))))) (odd? (lambda (n) (if (zero? n) #f (even? (- n 1)))))) (even? 4))  #t]
            [assert (letrec ((even? (lambda (n) (if (zero? n) #t (odd? (- n 1))))) (odd? (lambda (n) (if (zero? n) #f (even? (- n 1)))))) (even? 3))  #f]
            [assert (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))  70]

            [assert (case (* 2 3) ((2 3 5 7)   'prime) ((1 4 6 8 9) 'composite))  'composite]
            [assert (case (car '(c d)) ((a e i o u) 'vowel) ((w y) 'semivowel) (else 'consonant))  'consonant]

            [assert (case (car '(c d)) ((a) 'a) ((b) 'b))  (if #f #f)] ; -> NG

            [assert (cond ((> 3 2) 'greater) ((< 3 2) 'less))  'greater]
            [assert (cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))  'equal]
            [assert (cond ((assv 'b '((a 1) (b 2))) => cadr) (else #f))  2]

            [assert (let ((x '(1 3 5 7 9))) (do ((x x (cdr x)) (sum 0 (+ sum (car x)))) ((null? x) sum)))  25]

            [assert (+ 11 22 (call/cc (lambda (c) (* 44 (c 55)))))  88]

            [assert (quasiquote (a b c ,(+ 1 3)))   '(a b c 4)]
            [assert (quasiquote (a b c (+ 1 3)))    '(a b c (+ 1 3))]
            [assert (quasiquote (list ,(+ 1 2) 4))  '(list 3 4)]
            [assert (let ((n 99)) (quasiquote (list ,n ',n)))  '(list 99 (quote 99))]

            [assert (let-syntax ((mul2 (syntax-rules () ((_ arg) (* arg 2))))) (mul2 4))  8]
            [assert (let-syntax ((mul2 (syntax-rules () ((_ arg) (* arg 2)))) (pow2 (syntax-rules () ((_ arg) (* arg arg))))) (+ (mul2 4) (pow2 3)))  17]
            [assert (let-syntax ((*2 (syntax-rules() ((_ arg) (* 2 arg))))) (let-syntax ((*3 (syntax-rules() ((_ arg) (* 3 arg))))) (*2 (*3 6))))  36]
            [assert (letrec-syntax ((pow2 (syntax-rules() ((_ arg) (* arg arg)))) (pow4 (syntax-rules() ((_ arg) (* (pow2 arg) (pow2 arg)))))) (pow4 4))  256]
            [assert (letrec-syntax ((my-or (syntax-rules () ((my-or) #f) ((my-or e) e) ((my-or e1 e2 ...) (let ((temp e1)) (if temp temp (my-or e2 ...))))))) (my-or #f (+ 1 23)))  24]

            [assert (let loop ((lst '(1 2 3)) (acc 0)) (if (null? lst) acc (loop (cdr lst) (+ acc (car lst)))))  6]

            [assert (begin (define calc (lambda(x) (define m2 (lambda(n1)(* 2 n1))) (define p3 (lambda(n2)(+ 3 n2))) (p3 (m2 x)))) (calc 6))  15]
            [assert (cdr '(a . b))  'b]
            [assert (begin (define test0 (let ((n 0)) (lambda() (set! n (+ n 1)) n))) (test0) (test0) (test0))  3] ; if rename test0 into test , not passed.

            [assert (begin (define tak (lambda(x y z) (if (not (< y x)) z (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))))) (tak 2 1 0))  1]
            [assert (quasiquote (( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))  '((foo 7) . cons)]

            ))