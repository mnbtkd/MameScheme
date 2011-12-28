;;; -----------------------------------------------------------------
;;; evaluation
;;; -----------------------------------------------------------------
(define *builtin-fn-name* '(     * + - / < > zero? <= >= = not boolean? pair? cons car cdr set-car! set-cdr! null? list length symbol? symbol->string string->symbol
                                 char? char=? char<? char>? char<=? char>=? char->integer integer->char string? string-length make-string string-ref string-set!
                                 vector? make-vector vector-length vector-ref vector-set! number->string
                                 eq? eqv? equal? number? procedure? apply list? append memq memv assq assv assoc
                                 caar cadar cadddr caddr cadr cdar cdddr cddddr cddr string=? string-append substring map list-ref list-tail reverse
                                 list->vector vector-fill! vector->list string-copy
                                 caddddr caaaar caaadr caaar caadar caaddr caadr cadaar
                                 cadadr caddar cdaaar cdaadr cdaar cdadar cdaddr cdadr
                                 cddaar cddadr cddar cdddar member vector
                                 input-port? output-port? open-input-file open-output-file close-input-port close-output-port write display read-char write-char newline
                                 read for-each string->list string list->string string-fill!
                                 char-alphabetic? char-whitespace? char-lower-case? char-upper-case? char-numeric? char-upcase char-downcase char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
                                 string<? string>? string<=? string>=? string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
                                 standard-input-port standard-output-port current-input-port current-output-port
                                 load eof-object? gcd lcm even? odd? integer? modulo remainder quotient
                                 undefined
                                 )) ; TODO something wrong.. why is 'undefined' needed?
(define *builtin-fn*
  (let* ([procs            (list * + - / < > zero? <= >= = not boolean? pair? cons car cdr set-car! set-cdr! null? list length symbol? symbol->string string->symbol
                                 char? char=? char<? char>? char<=? char>=? char->integer integer->char string? string-length make-string string-ref string-set!
                                 vector? make-vector vector-length vector-ref vector-set! number->string
                                 eq? eqv? equal? number? procedure? apply list? append memq memv assq assv assoc
                                 caar cadar cadddr caddr cadr cdar cdddr cddddr cddr string=? string-append substring map list-ref list-tail reverse
                                 list->vector vector-fill! vector->list string-copy
                                 caddddr caaaar caaadr caaar caadar caaddr caadr cadaar
                                 cadadr caddar cdaaar cdaadr cdaar cdadar cdaddr cdadr
                                 cddaar cddadr cddar cdddar member vector
                                 input-port? output-port? open-input-file open-output-file close-input-port close-output-port write display read-char write-char newline
                                 read for-each string->list string list->string string-fill!
                                 char-alphabetic? char-whitespace? char-lower-case? char-upper-case? char-numeric? char-upcase char-downcase char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
                                 string<? string>? string<=? string>=? string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
                                 standard-input-port standard-output-port current-input-port current-output-port
                                 load eof-object? gcd lcm even? odd? integer? modulo remainder quotient
                                 undefined
                                 )]
         [prcs (make-vector (+ 3 (length procs)))])
    (vector-set! prcs 0 'type)
    (vector-set! prcs 1 'argnum)
    (vector-set! prcs 2 'body)
    (let loop ([i 0]
               [p procs])
      (unless (null? p)
        (vector-set! prcs (+ i 3) (car p))
        (loop (+ i 1) (cdr p)))
      )
    prcs))

(define run
  (lambda(x)
    (let ([envs (cons '() *builtin-fn-name*)])
      (trace #t "x   :~A~%envs:~A~%" x envs)
      (let ([code (compile (expand-qq (macroexpand x))
                           envs
                           '()
                           '(halt))])
        (trace #t "~%~A~%  =>~%~A~%" x code)
        (trace #t "~A~%" *builtin-fn*)
        (output-globals)
        (VM '() code 0 *builtin-fn* 0 0)))))

(define p1
  (lambda(x)
    (compile (expand-qq (macroexpand x))
             (cons '() *builtin-fn-name*)
             '()
             '(halt))))

(define p1-no-expand
  (lambda(x)
    (compile x
             (cons '() *builtin-fn-name*)
             '()
             '(halt))))

(define vmrun
  (lambda(code)
    (VM '() code 0 *builtin-fn* 0 0)))

(define macroexpand
  (lambda(x)
    (expand-rec x top-mark *syntaxes* #f))) ; TODO 最後の引数#fでいいのか
