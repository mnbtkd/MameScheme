(define test
  (lambda(code expected)
    (let ([flag *debug-flag*])
      (set! *debug-flag* #f)
      (let* ([result (run code)]
             [judge (equal? result expected)])
        (format #t "~A  ~A  =>  ~A"
                (if judge 'OK 'NG)
                code
                result)
        (if (not judge) (format #t "~%  expected [ ~A ]" expected)))
      (set! *debug-flag* flag))))

(define test-all
  (lambda(lst)
    (newline)
    (if (null? lst)
        '()
        (begin (test (caar lst)
                     (cdar lst))
               (test-all (cdr lst))))))

(define *test-cases*
  `([(quote a) . a] ; quote
    [(+ 4 5)   . 9] ; arithmetic
    [(* 2 3)   . 6]
    [(- 9 3)   . 6]
    [(/ 8 2)   . 4]
    [(/ (* 3 (+ 4 5)) (/ 6 (- 5 3))) . 9]
    [(begin (+ 1 2)(* 2 3)(/ 6 2))   . 3] ; begin
    [(if #t (+ 3 4)(+ 5 6)) . 7]
    [(if #f (+ 3 4)(+ 5 6)) . 11]
    [((lambda(x)(* x x)) 9)                  . 81]         ; func call
    [((lambda(x)
        (begin (set! x 88) x )) 77)          . 88]
    [((((lambda(x)
          (lambda(y)
            (lambda(z)(* z z)))) 11) 22) 33) . 1089]
    [(begin (define x 99) x)                 . 99]
    [(begin (define x 99)(set! x 88) x)      . 88] ; global assign
    [(begin (define x 99)(define x 77)(define y 66)(define y 55)(set! x 88)(set! y 77) x) . 88]
    [(begin (define a 9)(set! a 6)(* a a))           . 36]
    [(begin (define fn1 (lambda(x)(+ x x))) (fn1 6)) . 12]
    [(begin (define x 99)(define fn1 (lambda()(+ x x))) (fn1)) . 198]
    [(if (begin (define x 99) #t) (begin (set! x 88) x) (begin (set! x 77) x)) . 88]
    [(if (begin (define x 99) #f) (begin (set! x 88) x) (begin (set! x 77) x)) . 77]
    [(begin (define fn
              (lambda(n)
                (if (< 0 n)
                    (+ n (fn (- n 1)))
                    n)))
            (fn 3))                          . 6]
    [(begin (define fact
              (lambda(n)
                (if (> 1 n)
                    1
                    (* n (fact (- n 1))))))
            (fact 10))                       . 3628800]
    [(begin (define fact
              (lambda (n a)
                (if (> 1 n)
                    a
                    (fact (- n 1) (* a n )))))
            (fact 10 1)) . 3628800]
    [(begin (define fib
              (lambda (x)
                (if (< x 2)
                    1
                    (+ (fib (- x 1)) (fib (- x 2))))))
            (fib 10))                        . 89]
    [(if (or (< 2 1)(< 1 2))
         9
         8)                                  . 9]
    [(if (and (< 1 2)(< 2 1))
         9
         8)                                  . 8]
    [(if (and (< 1 2)(< 3 4))
         9
         8)                                  . 9]
    [(let ([a 1]
           [b 2])
       (+ a b))                              . 3]
    [(let ([fn (lambda(x)(* x x))])
       (fn 10))                              . 100]
    [(letrec ([even? (lambda (n)
                       (if (zero? n)
                           #t
                           (odd? (- n 1))))]
              [odd? (lambda (n)
                      (if (zero? n)
                          #f
                          (even? (- n 1))))])
       (even? 4))                            . #t]
    [(letrec ([even? (lambda (n)
                       (if (zero? n)
                           #t
                           (odd? (- n 1))))]
              [odd? (lambda (n)
                      (if (zero? n)
                          #f
                          (even? (- n 1))))])
       (even? 3))                            . #f]
    [(let ((x 2) (y 3))
       (let* ((x 7)
              (z (+ x y)))
         (* z x)))                           . 70]
    [(case (* 2 3)
       ((2 3 5 7)   'prime)
       ((1 4 6 8 9) 'composite))             . composite]
    [(case (car '(c d))
       ((a e i o u) 'vowel)
       ((w y) 'semivowel)
       (else 'consonant))                    . consonant]
    [(case (car '(c d))
       ((a) 'a)
       ((b) 'b))                             . ,(if #f #f)]
    [(cond ((> 3 2) 'greater)
           ((< 3 2) 'less))                  . greater]
    [(cond ((> 3 3) 'greater)
           ((< 3 3) 'less)
           (else 'equal))                    . equal]
    [(cond ((assv 'b '((a 1) (b 2))) => cadr)
           (else #f))                        . 2]
    [(let ((x '(1 3 5 7 9)))
       (do ((x x (cdr x))
            (sum 0 (+ sum (car x))))
           ((null? x) sum)))                 . 25]
    [(+ 11 22 (call/cc (lambda (c) (* 44 (c 55))))) . 88]
    [(quasiquote (a b c ,(+ 1 3)))           . (a b c 4)]
    [(quasiquote (a b c (+ 1 3)))            . (a b c (+ 1 3))]
    [(quasiquote (list ,(+ 1 2) 4))          . (list 3 4)]
    [(let ((n 99))
       (quasiquote (list ,n ',n)))     . (list 99 (quote 99))]
    [(cdr '(a . b)) . b]
    [((lambda x (car x)) '(a b c)) . (a b c)]
    [((lambda(x y . z) z) 1 2 3 4) . (3 4)]
    [(let-syntax ((mul2 (syntax-rules ()
                          ((_ arg)
                           (* arg 2)))))
       (mul2 4)) . 8]
    [(let-syntax ((mul2 (syntax-rules ()
                          ((_ arg)
                           (* arg 2))))
                  (pow2 (syntax-rules ()
                          ((_ arg)
                           (* arg arg)))))
       (+ (mul2 4)
          (pow2 3))) . 17]
    [(let-syntax ((*2 (syntax-rules()
                        ((_ arg)
                         (* 2 arg)))))
       (let-syntax ((*3 (syntax-rules()
                          ((_ arg)
                           (* 3 arg)))))
         (*2 (*3 6)))) . 36]
    [((lambda(a b c)
        (+ a b)
        (+ b c)
        (+ c a)) 1 2 3) . 4]
    [(quasiquote (( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))) . ((foo 7) . cons)]
    [(let loop ([lst '(1 2 3)]
                [acc 0])
       (if (null? lst)
           acc
           (loop (cdr lst)
                 (+ acc (car lst))))) . 6 ]
    [(letrec-syntax ((pow2 (syntax-rules()
                             ((_ arg)
                              (* arg arg))))
                     (pow4 (syntax-rules()
                             ((_ arg)
                              (* (pow2 arg)
                                 (pow2 arg))))))
       (pow4 4)) . 256]
    [(letrec-syntax
         ((my-or (syntax-rules ()
                   ((my-or) #f)
                   ((my-or e) e)
                   ((my-or e1 e2 ...)
                    (let ((temp e1))
                      (if temp
                          temp
                          (my-or e2 ...)))))))
       (my-or #f
              (+ 1 23))) . 24]
    [(begin
       (define calc
         (lambda(x)
           (define m2 (lambda(n1)(* 2 n1)))
           (define p3 (lambda(n2)(+ 3 n2)))
           (p3 (m2 x))))
       (calc 6)) . 15 ]

    [(begin
       (define tak
         (lambda(x y z)
           (if (not (< y x))
               z
               (tak (tak (- x 1) y z)
                    (tak (- y 1) z x)
                    (tak (- z 1) x y)))))
       (tak 10 3 0)) . 3]
    [(begin
       (define (mitfn a b)(+ a b))
       (mitfn 1 2)) . 3]
    [(begin (define test
         (let ([n 0])
           (lambda()
             (set! n (+ n 1))
             n)))
       (test)
       (test)
       (test)) . 3]
    ))

(test-all *test-cases*)

;(quote (a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
;=>
;(a 3 4 5 6 b)
;
;(quasiquote (( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))
;=>
;((foo 7) . cons)


