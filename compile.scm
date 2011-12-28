;;;; -----------------------------------------------
;;;; compile-time
;;;; -----------------------------------------------

(define syntax-check
  (lambda(x)
    (let ([key (car x)])
      (if (memq key '(quote
                      quasiquote
                      begin
                      lambda
                      if
                      set!
                      call/cc
                      define
                      define-syntax
                      let-syntax
                      letrec-syntax))
          (if (and (eq? key 'define)
                   (pair? (cadr x)))
              'define-mit
              key)
          'combination))))


;(define syntax-check  ; コンパイルできない
;  (lambda(x)
;    (let* ([key (car x)]
;           [cnd (assv key
;                      `((quote         . #f)
;                        (quasiquote    . #f)
;                        (begin         . #f)
;                        (lambda        . #f)
;                        (if            . #f)
;                        (set!          . #f)
;                        (call/cc       . #f)
;                        (define        . #f)
;                        (define-syntax . #f)
;                        (let-syntax    . #f)
;                        (letrec-syntax . #f)
;                        ))])
;      (if (not cnd)
;          'combination
;          key))))


;(define syntax-check
;  (lambda(x)
;    (let* ([key (car x)]
;           [cnd (assv key
;                      `((quote         . ,(lambda(x)(= 1 (length x))))
;                        (quasiquote    . ,(lambda(x)(= 1 (length x))))
;                        (begin         . ,(lambda(x) #t))
;                        (lambda        . ,(lambda(x)(< 0 (length x))))
;                        (if            . ,(lambda(x)(or (= 2 (length x))
;                                                        (= 3 (length x)))))
;                        (set!          . ,(lambda(x)(= 2 (length x))))
;                        (call/cc       . ,(lambda(x)(= 1 (length x)))) ; call/ccはsyntaxじゃないのでmalformed syntaxとするのは正しくない
;                        (define        . ,(lambda(x)(= 2 (length x))))
;                        (define-syntax . ,(lambda(x)(= 2 (length x))))
;                        (let-syntax    . ,(lambda(x)(= 2 (length x))))
;                        (letrec-syntax . ,(lambda(x)(= 2 (length x))))
;                        ))])
;      (if (not cnd)
;          'combination
;          (if ((cdr cnd)(cdr x))
;              key
;              (format #t "malformed syntax[~A]~%" key))))))

(define *ins-seq* 0)
(define mark-ins
  (lambda(sym)
    (set! *ins-seq* (+ 1 *ins-seq*))
    (string->symbol (string-append (symbol->string sym)
                                   "."
                                   (number->string *ins-seq*)))))
(define replace-top-sym
  (lambda(lst)
    (cond
     [(symbol? lst) (mark-ins lst)]
     [(pair?   lst) (cons (replace-top-sym (car lst))
                          (cdr lst))]
     [else lst])))
(define top-sym
  (lambda(lst)
    (cond
     [(pair?   lst) (top-sym (car lst))]
     [else lst])))
(define count-instructions-to-mark
  (lambda(lst sym)
    (let ([found (memq sym lst)]
          [whole (length lst)])
      (if found
          (- whole
             (length found))
          found))))




;;; -------------------------------------------------
;;; compile
;;; -------------------------------------------------
;;; x    --- コンパイル対象のコード
;;; e    --- 環境(ローカルと自由変数のペア) ((ローカル変数) . (自由変数))
;;; s    --- set! される変数
;;; next --- 直後に実行すべきコード
(define compile
  (lambda (x e s next)

    ;; argument style of function
    (define get-close-op
      (lambda(x)
        (cond [(symbol? x)                     'close1]    ; symbol as var
              [(and (pair? x) (not (list? x))) 'close2]    ; improper list as var
              [else                            'close0]))) ; normal list as var

    ;;; tail call?
    (define tail?
      (lambda (next)
        (eq? (car next) 'return)))

    ;;; boxing
    (define make-boxes
      (lambda (sets vars next)
        (let f ([vars vars] [n 0])
          (if (null? vars)
              next
              (if (set-member? (car vars) sets)
                  (cons 'box (cons n (f (cdr vars) (+ n 1))))
                  (f (cdr vars) (+ n 1)))))))

    ;(trace #t "--- compile ---------------~%x     ~A~%e     ~A~%s     ~A~%next  ~A~%glbl  ~A~%~%" x e s next *globals-n*)
    (cond
     [(symbol? x)
      (compile-refer
       x
       e
       (if (set-member? x s)
           (cons 'unbox next)
           next))]
     [(pair? x)
      (case (syntax-check x)
        ['quote
         (cons 'const (cons (cadr x) next))]
        ['quasiquote
         (compile (expand-qq-impl (cadr x)) e s next)]
        ['begin
         (begin
           (add-g* (find-define (cadr x)))
           (compile (cadr x) e s (if (null? (cddr x))
                                     next
                                     (compile (cons 'begin (cddr x)) e s next))))]
        ['lambda
         (let ([vars
                (let ([b (cadr x)])
                  (if (symbol? b)
                      (list b)
                      (flatten b)))]
               [op (get-close-op (cadr x))]
               [body (if (include-internal-def? (cddr x))
                         (convert-internal-def  (cddr x))
                         (if (not (null? (cdddr x)))
                             (list (cons 'begin (cddr x)))
                             (cddr x)))])
           (let ([free (find-free body
                                  vars
                                  (set-union (car e)    ; 現時点でのローカル変数と自由変数をfind-freeに渡す。
                                             (cdr e)))]
                 [sets (find-sets body vars)])
             ;(trace #t ">>> compiling lambda free[~A] sets[~A] env[~A] vars[~A] body[~A]~%" free sets e vars body)
             (let ([boxes (make-boxes sets
                                vars
                                (compile (car body)
                                         (cons vars free)
                                         (set-union sets
                                                    (set-intersect s free))
                                         (list 'return (length vars))))])
               (collect-free
                free
                e
                (cons op
                      (cons (length free)
                            (cons (length vars)
                                  (cons
                                   ;;(count-instructions boxes)
                                   (length boxes)
                                   (append boxes
                                           next)))))
                ))))]
        ['if
         (if (and (pair? next)
                  (tail? next)) ; tail-callの場合はthen節の最後でreturnすることが決まっているのでelse節をとばすためのjumpは要らない
             (let* ([elsec (if (= 2 (length (cdr x)))
                               (cons 'const (cons *undefined* next))
                               (compile (cadddr x) e s next))]
                    [thenc (append (compile (caddr x) e s next) elsec)])
               (compile (cadr x) e s (cons 'test
                                           (cons (- (length thenc)
                                                    (length elsec))
                                                 thenc))))
             (let* ([jump-to (replace-top-sym next)] ; jump先がわからなくならないように印をつける
                    [elsec (if (= 2 (length (cdr x)))
                               (cons 'const (cons *undefined* next))
                               (compile (cadddr x)
                                        e
                                        s
                                        jump-to))]
                    [thenc (compile (caddr x) e s (cons 'jump
                                                        (cons
                                                         (count-instructions-to-mark elsec (top-sym jump-to))
                                                         (replace elsec (list (cons (top-sym jump-to) (top-sym next)))))))]) ; つけた印をもとにもどす
               ;(format #t "NEXT(~A):~A~%" (count-instructions next)  next)(format #t "THEN(~A):~A~%" (count-instructions thenc) thenc)(format #t "ELSE(~A):~A~%" (count-instructions elsec) elsec)(format #t "JUMP: ~A~%" jump-to)
               (compile (cadr x) e s (cons 'test
                                           (cons (- (length thenc)
                                                    (length elsec))
                                                 thenc)))))]
        ['set!
         (let ([var (cadr x)]
               [x   (caddr x)])
           (begin
             ;(trace #t "compiling set! var[~A] x[~A] next[~A]~%" var x next)
             (compile-lookup var
                             e
                             (lambda (n)
                               (compile x e s (cons 'lset (cons n next))))
                             (lambda (n)
                               (compile x e s (cons 'fset (cons n next))))
                             (lambda (n)
                               (compile x e s (cons 'gset (cons n next)))))))]
        ['call/cc
         (let ([x (cadr x)])
           (let ([c (cons 'conti
                          (cons 'push
                                (compile x
                                         e
                                         s
                                         (if (tail? next)
                                             (list 'shift 1 (cadr next) 'call 0)
                                             (list 'call 1)))))])
             (if (tail? next)
                 c
                 (cons 'frame (cons (length c)
                                    ;;(count-instructions c)
                                    (append c next))))))]
;                                    (cons c next))))))]
        ['define
         (let ([var (cadr x)]
               [x   (caddr x)])
           (compile-define var
                           e
                           (lambda (n)
                             (compile x e s (cons 'gset (cons n next))))))]
        ['define-mit
         (let ([var (caadr x)]
               [x   (cons 'lambda (cons (cdadr x)(cddr x)))])
           (compile-define var
                           e
                           (lambda (n)
                             (compile x e s (cons 'gset (cons n next))))))]
        ['define-syntax
         (let ([var   (cadr x)]
               [rules (caddr x)])
           (compile (set-global-syntax var rules *syntaxes*) e s next))]
        ['let-syntax
            (let* ([env (let-syntax-imp x *syntaxes*)]
                   [expanded (expand-rec (caddr x) top-mark env #f)])
                (compile expanded e s next))]
        ['letrec-syntax
            (let* ([env (letrec-syntax-imp x *syntaxes*)]
                   [expanded (expand-rec (caddr x) top-mark env #t)])
                (compile expanded e s next))]
        ['combination
         ;(trace #t "COMBINATION COMPILING... ~A~%next  ~A~%" x next)
         (let loop ([arg (cdr x)]
                    [c (compile (car x) e s
                                (if (tail? next)
                                    (list 'shift
                                          (length (cdr x))
                                          (cadr next)
                                          'call
                                          (length (cdr x))
                                          (car next) ; return or return.??
                                          ;(cadr next)
                                          (length (cdr x))
                                          )
                                    (list 'call
                                          (length (cdr x))
                                          'return
                                          (length (cdr x))))
                                )])
           ;(trace #t "COMBINATION COMPILE LOOP~%    arg  ~A~%    c    ~A~%" arg c)
           (if (null? arg)
               (if (tail? next)
                   c
                   (append (list 'frame
                                 (length c)
                                 ;;(count-instructions c)
                                 )
                           c next))
               (loop (cdr arg)
                     (compile (car arg)
                              e
                              s
                              (cons 'push c)))))])]
     [else (cons 'const (cons x next))])))


;;;; --------------------------------------------
;;;; helper for compiler
;;;; --------------------------------------------

;TODO defineの節がない
(define find-free
  (lambda (x b f)
    ;(trace #t "find-free x[~A] b[~A] f[~A]~%" x b f)
    (cond
     [(symbol? x)
      (if (set-member? x b)
          '()
          (if (set-member? x f)
              (list x)
              '()))]
     [(pair? x)
      (case (car x)
        ['quote
         '()]
        ['quasiquote
         '()]
        ['begin
         (set-union (find-free (cadr x) b f)
                    (find-free (cddr x) b f))]
        ['lambda
         (find-free (cddr x)
                    (set-union (flatten (cadr x)) b)
                    f)] ; TODO 暗黙のbeginに対応してない?
        ['if
         (set-union (find-free (cadr x) b f)
                    (set-union (find-free (caddr x) b f)
                               (if (pair? (cdddr x))
                                   (find-free (cadddr x) b f)
                                   '())))]
        ['set!
         (set-union (if (set-member? (cadr x) b)
                        '()
                        (if (set-member? x f)
                            (list (cadr x))
                            '()))
                    (find-free (caddr x) b f))]
        ['call/cc
         (find-free (cadr x) b f)]
        [else
         (let next ([x x])
           ;(trace #t "find-free next~%")
           (if (null? x)
               '()
               (set-union (find-free (car x) b f)
                          (next (cdr x)))))])]
     [else '()])))

(define find-sets
  (lambda (x v)
    ;(trace #t "find-sets x[~A] v[~A]~%" x v)
    (cond
     [(symbol? x) '()]
     [(pair? x)
      (case (car x)
        ['quote
         '()]
        ['begin
         (set-union (find-sets (cadr x) v)
                    (find-sets (cddr x) v))]
        ['lambda
                     (find-sets (cddr x) (set-minus v (flatten (cadr x))))]
        ['if
         (set-union (find-sets (cadr x) v)
                    (set-union (find-sets (caddr x) v)
                               (if (pair? (cdddr x))
                                   (find-sets (cadddr x) v)
                                   '())))]
        ['set!
         (set-union (if (set-member? (cadr x) v)
                        (list (cadr x))
                        '())
                    (find-sets (caddr x) v))]
        ['call/cc
         (find-sets (cadr x) v)]
        ['define
         (set-union (if (set-member? (cadr x) v)
                        (list (cadr x))
                        '())
                    (find-sets (caddr x) v))]
        [else
         (let next ([x x])
           (if (null? x)
               '()
               (set-union (find-sets (car x) v)
                          (next (cdr x)))))])]
     [else '()])))

(define find-define
  (lambda(x)
    ;(trace #t "find-defn x[~A]~%" x)
    (cond
     [(symbol? x) '()]
     [(null? x)   '()]
     [(pair? x)
      (case (car x)
        ['quote
         '()]
        ['begin
         (set-union (find-define (cadr x))
                    (find-define (cddr x)))]
        ['lambda
         (find-define (cddr x))] ; TODO body部の暗黙のbeginに対応する
        ['if
         (set-union (find-define (cadr x))
                    (set-union (find-define (caddr x))
                               (if (pair? (cdddr x))
                                   (find-define (cadddr x))
                                   '())))]
        ['set!
         (set-union (find-define (cadr x))
                    (find-define (caddr x)))]
        ['call/cc
         (find-define (cadr x))]
        ['define
         (list (cadr x))]
        [else
         (let next ([x x])
           (if (null? x)
               '()
               (set-union (find-define (car x))
                          (next (cdr x)))))])]
     [else '()])))

(define compile-refer
  (lambda(x e next)
    ;(trace #t "compile-refer  x[~A]  e[~A]  next[~A]~%" x e next)
    (compile-lookup x e
                    (lambda (n) (cons 'lref (cons n next)))
                    (lambda (n) (cons 'fref (cons n next)))
                    (lambda (n) (cons 'gref (cons n next))))))

(define compile-lookup
  (lambda (x e return-local return-free return-global)
    ;(trace #t "compile-lookup x[~A]  e[~A]~%" x e)
    (let nxtlocal ([locals (car e)]
                   [n 0])
      (if (null? locals)
          (let nxtfree ([free (cdr e)]
                        [n 0])
            (cond [(null? free)
                   (begin ;(trace #t "as global~%~%")
                          (return-global (add-g x)))]
                  [(eq? (car free) x)
                   (begin ;(trace #t "as free~%~%")
                          (return-free n))]
                  [else
                   (nxtfree (cdr free) (+ n 1))]))
          (if (eq? (car locals) x)
              (begin
                ;(trace #t "as local~%~%")
                (return-local n))
              (nxtlocal (cdr locals) (+ n 1)))))))

(define compile-define
  (lambda(var e next)
    (next (add-g var))))

(define collect-free
  (lambda (vars e next)
    ;(trace #t "collect-free   v[~A]  e[~A] next[~A]~%" vars e next)
    (if (null? vars)
        next
        (collect-free (cdr vars)
                      e
                      (compile-refer (car vars)
                                     e
                                     (cons 'push next))))))


;;;; ------------------------------------------------
;;;; internal define
;;;; ------------------------------------------------
(define convert-internal-def
  (lambda(x)
    (define convert
      (lambda(x)
        (cond [(include-internal-def? x)
               (cons (cdar x)
                     (convert (cdr x)))]
              [else '()])))
    (define internal-def-body
      (lambda(x)
        (cond [(include-internal-def? x)
               (internal-def-body (cdr x))]
              [else x])))
    (list (macroexpand ; TODO コンパイラからmacroexpandはあまり呼びたくない。この場合、letrecを展開させることが目的なので、あらかじめletrecの展開形を組み立てるようにする
            (cons 'letrec
                  (cons (convert x)
                        (internal-def-body x)))))))

(define include-internal-def?
  (lambda(x)
    (and (pair? x)
         (pair? (car x))
         (eq? (caar x) 'define))))


;;;; ------------------------------------------------
;;;; quasiquote
;;;; ------------------------------------------------

(define expand-qq
  (lambda(x)
    ;(trace #t "[EXPAND-QQ] ~A~%" x)
    (cond [(pair?   x)
           (cond [(and (eqv? 'quasiquote (car x))
                       (pair? (cdr x)))
                  (expand-qq-impl (cadr x))]   ;  (compile (back-qsyms (qq (alt-qsyms (cadr x)) 1 0)) e s next)]
                 [(eqv? 'quote (car x))
                  x]
                 [else
                  (cons (expand-qq (car x))
                        (expand-qq (cdr x)))])]
          [else x])))


(define alt-qsyms
  (lambda(x)
    (replace x '((quote            . __Q__ )
                 (unquote          . __UNQ__)
                 (unquote-splicing . __UQS__)
                 (quasiquote       . __QQ__)))))
(define back-qsyms
  (lambda(x)
    (replace x '((__Q__   . quote)
                 (__UNQ__ . unquote)
                 (__UQS__ . unquote-splicing)
                 (__QQ__  . quasiquote)))))
(define qq
  (lambda(x qql uql)
    (cond [(null? x)
           '()]
          [(and (pair? x)(uqs? (car x)))
           (cons 'append (qqi x qql uql))]
          [else
           (cons 'cons (qqi x qql uql))])))
(define qqi
  (lambda(x qql uql)
    (cond
     [(and (pair? x)               ; (unquote x) or (unquote-splicing x)
           (or (unq? x)
               (uqs? x)))
      (if (= qql (+ 1 uql))
          (cadr x)
          (cons 'cons
                (cons (list 'quote 
                            (cond [(unq? x) 'unquote]
                                  [(uqs? x) 'unquote-splicing]))
                      (cons (cons 'cons (qqi (cdr x) qql (+ 1 uql)))
                            '()))))]
     [(and (pair? x)               ; (quote x)
           (q? x))
      (list 'list ''quote (qqi (cadr x) qql uql))]
     [(and (pair? x)               ; (quasiquote x)
           (qq? x))
      (cons (qqi 'quasiquote qql uql)
            (list (qq (cdr x) (+ 1 qql) uql)))]
     [(and (pair? x)               ; ((x y) z)
           (pair? (car x))
           (not (unq? (car x)))
           (not (uqs? (car x)))
           (not (q? (car x))))
      (cons (qq (car x) qql uql)
            (list (qq (cdr x) qql uql)))]
     [(pair? x)                    ; (x y z)
      (cons (qqi (car x) qql uql)
            (if (or (unq? (cdr x))
                    (uqs? (cdr x))
                    (q?   (cdr x))
                    (qq?  (cdr x)))
                (list (qqi (cdr x) qql uql))
                (list (qq (cdr x) qql uql))))]
     [else
      (list 'quote x)]        ; x
     )))
(define unq?
  (lambda(x)
    (cond
     [(pair? x)
      (eqv? '__UNQ__ (car x))]
     [else #f])))
(define uqs?
  (lambda(x)
    (cond
     [(pair? x)
      (eqv? '__UQS__ (car x))]
     [else #f])))
(define qq?
  (lambda(x)
    (cond
     [(pair? x)
      (eqv? '__QQ__ (car x))]
     [else #f])))
(define q?
  (lambda(x)
    (cond
     [(pair? x)
      (eqv? '__Q__ (car x))]
     [else #f])))

(define expand-qq-impl
  (lambda(x)
    (back-qsyms (qq (alt-qsyms x) 1 0))))



;;;; ------------------------------------------------
;;;; global
;;;; ------------------------------------------------
(define *globals-n* '())
(define add-g*
  (lambda(lst)
    (if (pair? lst)
        (begin (add-g (car lst))
               (add-g* (cdr lst)))
        '())))
(define add-g
  (lambda(x)
    (define push-g
      (lambda(x)
        (set! *globals-n* (cons x *globals-n*))
        (- (length *globals-n*) 1)))
    (if (memq x *globals-n*)
        (- (length (memq x *globals-n*)) 1)
        (push-g x))))



