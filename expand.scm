;;; -----------------------------------------------------------------
;;; macro
;;; -----------------------------------------------------------------

;; syntax-rules --------------------------------

(define set-global-syntax
  (lambda(name x env)
;;    (trace #t "[SET-GLOBAL-SYNTAX]~%  name : ~A~%  rules: ~A~%" name x)
    (let ([defpair (make-syntax-def name x '())]
          [r       (assq name *syntaxes*)])
;;      (trace #t "  defpair : ~A~%" defpair)
      (if r
          (set-cdr! r (cdr defpair))
          (set! *syntaxes*
                (cons defpair
                      *syntaxes*)))
      (if #f #t) ; #<undef>を返したいので
      )))

(define let-syntax-imp
  (lambda(x env)
    (let loop ([bindings (cadr x)]
               [newenv '()])
      (if (null? bindings)
          (append newenv env)
          (let ([keyword  (caar bindings)]
                [trnsfrm  (cadar bindings)])
            (loop (cdr bindings)
                  (cons (make-syntax-def keyword
                                         trnsfrm
                                         env) newenv)))))))

(define letrec-syntax-imp
  (lambda(x env)
    (define backpatch-env!
      (lambda(x env)
        (if (not (null? x))
            (begin
              (if (eqv? '%PLACEHOLDER%
                        (car (cddddr (car x))))
                  (set-cdr! (cddddr (car x)) env)
                  *undefined*)
              (backpatch-env! (cdr x)
                              env))
            *undefined*)))
    (let loop ([bindings (cadr x)]
               [newenv '()])
      (if (null? bindings)
          (let ([x (append newenv env)])
            (backpatch-env! x x)
            x)
          (let ([keyword  (caar bindings)]
                [trnsfrm  (cadar bindings)])
            (loop (cdr bindings)
                  (cons (make-syntax-def keyword
                                         trnsfrm
                                         '%PLACEHOLDER%) newenv)))))))

(define make-syntax-def
  (lambda(name x env)
    (let ([syms  (cadr x)]
          [patterns  (map car  (cddr x))]
          [templates (map cadr (cddr x))])
      (let ([def (list syms patterns templates env)])
        (cons name def)
              ))))








(define expand
  (lambda(x mark env rec?)


    (define ellipsis?
      (lambda(x)
        (and (pair? x)
             (pair? (cdr x))
             (eqv?  (cadr x) '...))))

    ;; TODO きちんと一意なシンボルを返すようにする。
    (define gen-variety
      (lambda(x d)
        ;;(format #t "gen-variety   ~A~%" x)
        (cond [(symbol? x)
               (if (eqv? x '...)
                   '...
                   (string->symbol (string-append (symbol->string x)
                                                  "_"
                                                  ;;(number->string (suffix-num))
                                                  (number->string d)
                                                  )))]
              [(pair? x)
               (if (ellipsis? x)
                   (cons (gen-variety (car x) (+ 1 d))
                         (gen-variety (cdr x) d))
                   (cons (gen-variety (car x) d)
                         (gen-variety (cdr x) d)))]
              [else x])))

    (define gen-variety2
      (lambda(x d lst)
        ;;(format #t "gen-variety2  ~A [~A]~%" x lst)
        (cond [(symbol? x)
               (if (eqv? x '...)
                   '...
                   (let ([new (string->symbol
                               (string-append (symbol->string x)
                                              "_"
                                              ;;(number->string (suffix-num))
                                              (number->string d)))])
                     (if (and (memq x lst) (memq new lst))
                         new
                         x)))]
              [(pair? x)
               (if (ellipsis? x)
                   (cons (gen-variety2 (car x) (+ 1 d) lst)
                         (gen-variety2 (cdr x) d lst))
                   (cons (gen-variety2 (car x) d lst)
                         (gen-variety2 (cdr x) d lst)))]
              [else x])))

    (define add-family
      (lambda(x)
        (set! *family* (set-union x *family*))
        *family*))

    (define pmatch
      (lambda(x y lits)
;;        (trace #t "[PMATCH]~%  x : ~A~%  y : ~A~%  l : ~A~%" x y lits)
        (let ([family '()])
          (let ([add-family
                 (lambda(x)
                   (set! family
                         (set-union x family))
                   family)])
            (pm-impl x y lits '() '())))))

    (define varsym?
      (lambda(x lits)
        (and (symbol? x) (not (memq x lits)))))

    (define pm-impl
      (lambda (x y lits cand bn)
        (cond [(or (eqv? x y)
                   (eqv? x '_)
                   (eqv? y '_))
               (begin (add-family cand)
                      (cons #t bn))]   ; 新たにバインディングは生じない
              [(varsym? x lits)      ; => xが変数だった
               (begin (add-family cand)
                      (cons #t (cons (cons x y) bn)))]  ; (x . y)というマッチ情報を追加
              [(and (pair? x)
                    (pair? y))
               (let ([k (pm-impl (car x) (car y) lits cand bn)]) ; => x も y もpairで car部がマッチしたら...
                 (if (car k)
                     (if (ellipsis? x)
                         (pm-impl (cons (gen-variety (car x) 0)
                                        (cdr x))
                                  (cdr y)
                                  lits
                                  (append (flatten (car x)) cand)
                                  (cdr k))
                         (pm-impl (cdr x) (cdr y) lits cand (cdr k)))
                     (cons #f #f)))]
              [(and (null? y)
                    (ellipsis? x))
               (begin (add-family cand)
                      (cons #t bn))]
              [#t (begin (add-family cand)
                         (cons #f #f))]
              )))

    (define transform
      (lambda(template bind)
        ;;(format #t "transform~%  t:~A~%  b:~A~%" template bind)
        (cond [(symbol? template)
               (let ([r (assq template bind)])
                 (if r
                     (cdr r)
                     template))]
              [(ellipsis? template) ; (xxx ...)
               (let ([transformed (transform (car template) bind)])
                 ;;(format #t "  ellipsis [~A] => [~A]~%" (car template) transformed)
                 (if (equal? (car template) transformed) ; xxx部分が変換前と変換後でかわらないので…
                     (if (pair? (cdr template))
                         (transform (cddr template ) bind)) ; (xxx ...)の後を変換。
                     (cons transformed                   ; xxx部分は変換された。
                           (transform 
                            (let ([generated (gen-variety2 (car template) 0 *family*)])
                              ;;(format #t "gen-variety2 [~A] => [~A]~%" (car template) generated)
                              (if (equal? generated (car template))
                                  (if (pair? (cdr template))
                                      (transform (cddr template ) bind))
                                  (cons generated
                                        (cdr template))))
                            bind))))]
              [(pair? template)
               (cons (transform (car template) bind)
                     (transform (cdr template) bind))]
              [else template])))


;;    (trace #t "[EXPAND]~%  x : ~A~%  m : ~A~%  e : ~A~%" x mark (macro-names env))
    (let* ([stript (newstrip (car x))]
           [lits   (literals stript env)]
           [isom   (synobj-isomorph (normalize-synobj (add-mark mark x))
                                    lits)])
      ;;(trace #t "  isom : ~A~%" isom)
      (let loop ([ptn  (patterns  stript env)]
                 [tmpl (templates stript env)])
;;        (trace #t "  expand~%  x ~A~%  l ~A~%  p ~A~%  t ~A~%  m ~A~%  i ~A~%" x lits ptn tmpl mark isom)
        (if (null? ptn)
            x
            (let ([r (pmatch (car ptn)
                             (cdr isom) lits)])
              (if (car r)
                  (expand-rec
                   (newstrip
                    (normalize-synobj
                     (add-mark mark
                               (transform (transform (car tmpl)
                                                     (cdr r))
                                          (car isom)))))
                   mark
                   (if rec?
                       env
                       (syntax-env stript env))
                   rec?)
                  (loop (cdr ptn)
                        (cdr tmpl)))))))))

(define expand-rec
  (lambda(x mark env rec?)

   (define macro?
      (lambda(x env)
        (let ([exp (syntax-object-expr x)])
          (and (pair? exp)
               (find-syntax (car exp) env)))))

    (define quote?
      (lambda(x)
        (and (pair? x)
             (eqv? 'quote (car x)))))

;;    (trace #t "[EXPAND-REC]~%  x : ~A~%  m : ~A~%  s : ~A~%  e : ~A~%" x mark (newstrip x) (macro-names env))
    (cond [(symbol? x) x]
          [(quote?  x) x]
          [(pair?   x)
           (cond [(binding-exp? x)
                  (expand-binding x mark env rec?)]
                 [(macro? x env)
                  (let ([newmark (make-mark)])
                    (expand x newmark env rec?))]
                 [(or (eqv? 'define-syntax (car x))
                      (eqv? 'syntax-rules (car x)))
                  x ]
                 [(syn-identifier? x) x]
                 [else
                  (cons (expand-rec (car x) mark env rec?)
                        (expand-rec (cdr x) mark env rec?))])]
          [else x])))


(define newstrip
  (lambda(x)

    (define replace-sym
      (lambda(sym wrap)
        ;(format #t "replace-sym   s:~A~%   w:~A~%" sym wrap)
        (let ([mark* (get-mark*  wrap '())]
              [sbst* (get-subst* wrap '())])
          ;(format #t "   m*:~A  s*:~A~%" mark* sbst*)
          (let search ([sbst* sbst*])
            (if (null? sbst*)
                sym
                (let ([sbst (car sbst*)])
                  (if (and (eq? sym (subst-sym sbst))
                           (memq (subst-mark* sbst) mark*))
                      (subst-label sbst)
                      (search (cdr sbst*)))))))))
    (define get-mark*
      (lambda(wrap acc)
        (if (null? wrap)
            acc
            (let ([w (car wrap)])
              (if (mark? (car wrap))
                  (get-mark* (cdr wrap)
                             (cons (car wrap) acc))
                  (get-mark* (cdr wrap) acc))))))
    (define get-subst*
      (lambda(wrap acc)
        (if (null? wrap)
            acc
            (let ([w (car wrap)])
              (if (subst? (car wrap))
                  (get-subst* (cdr wrap)
                              (cons (car wrap) acc))
                  (get-subst* (cdr wrap) acc))))))
    (cond [(syntax-object? x)
           (if (syn-identifier? x)
               (replace-sym (syntax-object-expr x)
                            (syntax-object-wrap x))
               (newstrip (syntax-object-expr x)))]
          [(pair? x)
           (cons (newstrip (car x))
                 (newstrip (cdr x)))]
          [else x])))



(define *family* '())

(define *syntaxes* '())

(define find-syntax
  (lambda(x env)
    (assq x env)))

(define literals
  (lambda(x env)
    (cadr (find-syntax x env))))

(define patterns
  (lambda(x env)
    (caddr (find-syntax x env))))

(define templates
  (lambda(x env)
    (cadddr (find-syntax x env))))

(define syntax-env
  (lambda(x env)
    (let ([e (car (cddddr (find-syntax x env)))])
      (if (null? e)
          *syntaxes*  ; 何も指していないというのはグローバルを指しているということ
          e))))

;; syntax-object
(define make-syntax-object
  (lambda(exp wrap)
    (if (self-evaluating? exp)
        exp
        (cons '*syntax-object* (cons exp wrap)))))

(define syntax-object?
  (lambda(x)
    (and (pair? x)
         (eq? (car x) '*syntax-object*)
         (pair? (cdr x)))))

(define syntax-object-expr
  (lambda(x)
;;    (trace #t "syntax-object-expr : ~A~%" x)
    (if (syntax-object? x)
        (cadr x)
        x)))
(define syntax-object-wrap
  (lambda(x)
    (if (syntax-object? x)
        (cddr x)
        x)))

;; mark
(define *make-mark-num* 0)
(define make-mark
    (lambda()
      (set! *make-mark-num* (+ *make-mark-num* 1))
      (string->symbol (string-append "mark." (number->string *make-mark-num*)))))

(define mark?
  (lambda(x)
    (and (symbol? x)
         (let ([str (symbol->string x)])
           (and (<= 5 (string-length str))
                (string=? (substring str 0 5)
                          "mark."))))))

(define top-mark (make-mark))

(define top-marked?
  (lambda (wrap)
    (and (not (null? wrap))
         (or (eq? (car wrap) top-mark)
             (top-marked? (cdr wrap))))))

;; substitute
(define make-subst
  (lambda(id mark* label)
    ;(format #t "make-subst~%  id : ~A~%  mk : ~A~%  lb : ~A~%" id mark* label)
    (cons '*substitute*
          (cons id (cons mark* label)))))
(define subst?
  (lambda(x)
    (and (pair? x)
         (eq? (car x) '*substitute*))))
(define subst-sym
  (lambda(x)
    (if (subst? x)
        (cadr x)
        x)))
(define subst-mark*
  (lambda(x)
    (if (subst? x)
        (caddr x)
        x)))
(define subst-label
  (lambda(x)
    (if (subst? x)
        (cdddr x)
        x)))

(define add-mark
  (lambda (mark x)
    (extend-wrap (list mark) x)))

(define extend-wrap
  (lambda (wrap x)

    (define join-wraps
      (lambda (wrap1 wrap2)
        (cond
         [(null? wrap1) wrap2]
         [(null? wrap2) wrap1]
         [else
          (let f ([w (car wrap1)] [w* (cdr wrap1)])
            (if (null? w*)
                (if (and (mark? w) (eq? (car wrap2) w))
                    (cdr wrap2)
                    (cons w wrap2))
                (cons w (f (car w*) (cdr w*)))))])))

    (if (syntax-object? x)
        (make-syntax-object
         (syntax-object-expr x)
         (join-wraps wrap (syntax-object-wrap x)))
        (make-syntax-object x wrap))))

(define normalize-synobj
  (lambda(x)
    (if (syntax-object? x)
        (let ([exp (syntax-object-expr x)]
              [wrp (syntax-object-wrap x)])
          ;(format #t "normalize-synobj  exp : ~A~%  wrp : ~A~%" exp wrp)
          (cond [(null? exp) '()]
                [(pair? exp)
                 (cons (normalize-synobj (extend-wrap wrp (car exp)))
                       (normalize-synobj (extend-wrap wrp (cdr exp))))]
                [else (extend-wrap wrp exp)]))
        x)))

(define synobj-isomorph
  (lambda(x lits)

    (define synobj-id
      (let ([n 0])
        (lambda()
          (set! n (+ 1 n))
          (string->symbol
           (string-append
            "syn."
            (number->string n))))))

    (define synobj-isomorph-impl
      (lambda(exp lits)
        (let ([x (cdr exp)]
              [b (car exp)])
          (cond [(null? x) (cons b '())]
                [(and (pair? x)
                      (syntax-object? x))
                 (let ([id  (synobj-id)]
                       [sym (syntax-object-expr x)])
                   (if (memq sym lits)
                       (cons (cons (cons sym x) b) sym)
                       (cons (cons (cons id  x) b) id)
                       ))]
                [(pair? x)
                 (let ([kar (synobj-isomorph-impl (cons b (car x))
                                                  lits)]
                       [kdr (synobj-isomorph-impl (cons b (cdr x))
                                                  lits)])
                   (cons (append b
                                 (car kar)
                                 (car kdr))
                         (cons (cdr kar)
                               (cdr kdr))))]
                [else (cons b x)]))))

    (synobj-isomorph-impl (cons '() x)
                          lits)))

(define self-evaluating?
  (lambda (x)
    (or (boolean? x)
        (number?  x)
        (string?  x)
        (char?    x))))

(define binding-exp?
  (lambda(x)
    ;(format #t "binding-exp?  ~A~%" x)
    (cond [(syntax-object? x)
           (binding-exp? (syntax-object-expr x))]
          [(pair? x)
           (or (and (syntax-object? (car x))
                    (eq? (syntax-object-expr (car x)) 'lambda))
               (and (eq? (car x) 'lambda)
                    (pair? (cdr x))))]
          [else #f]
          )))

(define syn-identifier?
  (lambda (x)
    (and (syntax-object? x)
         (symbol? (syntax-object-expr x)))))

(define *gen-var-num* 0)
(define gen-var
  (lambda (id)
    (set! *gen-var-num* (+ *gen-var-num* 1))
    (let ([name (syntax-object-expr id)])
      (string->symbol ;(format "~s.~s" name n)
       (string-append (symbol->string name)
                      "."
                      (number->string *gen-var-num*))))))

(define expand-binding
  (lambda(x mark env rec?)

    (define add-subst
      (lambda (id mark label x)
        ;(format #t "add-subst~%  id : ~A~%  mk : ~A~%  lb : ~A~%  x  : ~A~%" id mark label x)
        (extend-wrap (list (make-subst
                            (syntax-object-expr id)
                            mark
                            label))
                     x)))

    (define add-subst-and-mark
      (lambda(bind exp mark)
        ;(format #t "add-subst-and-mark~%  ~A~%  ~A~%" bind exp)
        (if (null? bind)
            (normalize-synobj (add-mark mark exp))
            (add-subst-and-mark (cdr bind)
                                (add-subst (car bind)
                                           mark
                                           (gen-var (syntax-object-expr (car bind)));labelじゃなくて直接新しいシンボルをくっつけとく
                                           exp)
                                mark))))
    (define get-bind
      (lambda(x)
        ;(format #t "get-bind ~A~%" x)
        (let ([bind (syntax-object-expr (cadr x))])
          (if (symbol? bind)
              (list bind)
              (flatten bind)))))

    ;(format #t "expand-binding~%  x : ~A~%  m : ~A~%" x mark)
    (let ([newmark (make-mark)]
          [bind    (get-bind x)])
      (let ([expanded
             (newstrip (add-subst-and-mark (syntax-object-expr bind)
                                           (newstrip x)
                                           newmark
                                           ))])
        (cons (car expanded)
              (cons (cadr expanded)
                    (expand-rec (cddr expanded) newmark env rec?)
                    ))))))

(define macro-names
  (lambda(x)
    (map car x)))

