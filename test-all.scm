(define print
  (lambda exp
    (if (not (null? exp))
        (begin (display (car exp))
               (apply print (cdr exp))))))

(define puts
  (lambda exp
    (if (not (null? exp))
        (begin (display (car exp))
               (newline)
               (apply puts (cdr exp))))))

(define-syntax assert
  (syntax-rules ()
    ((_ expr  rslt)
     (let [(r0 expr)]
       (if (equal? r0 rslt)
           (begin (display ".")
                  (set! *passed* (cons (quote expr) *passed*)))
           (begin (display "X")
                  (set! *failed* (cons (quote expr) *failed*))))
       ))))

(define *passed* '())
(define *failed* '())

(define-syntax run-test
  (syntax-rules ()
    ((_ title tests)
     (begin
       (set! *passed* '())
       (set! *failed* '())
       (newline)
       (display title)
       tests
       (newline)
       (show-results)
       ))))

(define show-results
  (lambda()
    ;(newline)
    (display (length *passed*))
    (display " passed, ")
    (display (length *failed*))
    (display " failed.")
    (newline)
    (show-failures *failed*)
    (display "--------------------------")
    (newline)))

(define show-failures
  (lambda(f)
    (if (not (null? f))
        (begin (display "failure : ")
               (display (car f))
               (newline)
               (show-failures (cdr f))))))

(load "./test/test_equivalence.scm")
(load "./test/test_string.scm")
(load "./test/test_vector.scm")
(load "./test/test_syntax.scm")
(load "./test/test_bignumber.scm")
(load "./test/test_list.scm")
(load "./test/test_arithmetic.scm")
(load "./test/test_char.scm")
(load "./test/test_type_conversion.scm")
(load "./test/test_misc.scm")
