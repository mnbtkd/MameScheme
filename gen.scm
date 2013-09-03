;; util
(load "./util.scm")
;; macro
(load "./expand.scm")
;; compile
(load "./compile.scm")
;; run-time
(load "./vm.scm")
;; evaluation
(load "./eval.scm")
;; library syntax
(load "./lib_syn.scm")

(define bootstrap
  (lambda(fname . no-act)
    (call-with-input-file fname
      (lambda(p)
        (let loop ([sexp (read p)])
          (if (eof-object? sexp)
              'OK
              (begin
                (format #t "~S~%" sexp )
                (if (null? no-act)
                    (run sexp))
                (loop (read p)))))))))

(bootstrap "./util.scm")
(bootstrap "./expand.scm")
(bootstrap "./compile.scm")
(bootstrap "./vm.scm")
(bootstrap "./eval.scm")
(bootstrap "./lib_syn.scm")
(bootstrap "./lib_prc.scm")


(define immediate-subr
  (lambda(x)
    (let ([exp x])
      (cond
       [(equal? exp "#<closure cdadr>")                 "subr_cdadr"]
       [(equal? exp "#<closure caadr>")                 "subr_caadr"]
       [(equal? exp "#<closure cadar>")                 "subr_cadar"]
       [(equal? exp "#<closure cadddr>")                "subr_cadddr"]
       [(equal? exp "#<closure caddr>")                 "subr_caddr"]
       [(equal? exp "#<closure cddddr>")                "subr_cddddr"]
       [(equal? exp "#<closure cdddr>")                 "subr_cdddr"]
;       [(equal? exp "#<closure format>")                "subr_format"]
       [(equal? exp "#<closure format>")                "(void*)SCH_NIL"]
       [(equal? exp "#<subr +>")                        "subr_add"]
       [(equal? exp "#<subr ->")                        "subr_sub"]
       [(equal? exp "#<subr <=>")                       "subr_is_equal_to_or_greater"]
       [(equal? exp "#<subr <>")                        "subr_is_greater"]
       [(equal? exp "#<subr =>")                        "subr_is_equal_to_or_less"]
       [(equal? exp "#<subr append>")                   "subr_append"]
       [(equal? exp "#<subr apply>")                    "subr_apply"]
       [(equal? exp "#<subr assq>")                     "subr_assq"]
       [(equal? exp "#<subr boolean?>")                 "subr_is_boolean"]
       [(equal? exp "#<subr caar>")                     "subr_caar"]
       [(equal? exp "#<subr cadr>")                     "subr_cadr"]
       [(equal? exp "#<subr car>")                      "subr_car"]
       [(equal? exp "#<subr cdar>")                     "subr_cdar"]
       [(equal? exp "#<subr cddr>")                     "subr_cddr"]
       [(equal? exp "#<subr cdr>")                      "subr_cdr"]
       [(equal? exp "#<subr char=?>")                   "subr_char_is_equal"]
       [(equal? exp "#<subr char?>")                    "subr_is_char"]
       [(equal? exp "#<subr cons>")                     "subr_cons"]
       [(equal? exp "#<subr eq?>")                      "subr_is_eq"]
       [(equal? exp "#<subr equal?>")                   "subr_is_equal"]
       [(equal? exp "#<subr eqv?>")                     "subr_is_eqv"]
       [(equal? exp "#<subr length>")                   "subr_length"]
       [(equal? exp "#<subr list>")                     "subr_list"]
       [(equal? exp "#<subr list?>")                    "subr_is_list"]
       [(equal? exp "#<subr make-string>")              "subr_make_string_k"]
       [(equal? exp "#<subr make-vector>")              "subr_make_vector"]
       [(or (equal? exp "#<closure map>")
            (equal? exp "#<subr map>"))                 "subr_map"]
       [(equal? exp "#<subr memq>")                     "subr_memq"]
       [(equal? exp "#<subr memv>")                     "subr_memv"]
       [(equal? exp "#<subr not>")                      "subr_not"]
       [(equal? exp "#<subr null?>")                    "subr_is_null"]
       [(equal? exp "#<subr number->string>")           "subr_number2string"]
       [(equal? exp "#<subr number?>")                  "subr_is_number"]
       [(equal? exp "#<subr pair?>")                    "subr_is_pair"]
       [(equal? exp "#<subr procedure?>")               "subr_is_procedure"]
       [(equal? exp "#<subr set-cdr!>")                 "subr_set_cdr"]
       [(equal? exp "#<subr string->symbol>")           "subr_string2symbol"]
       [(equal? exp "#<subr string-append>")            "subr_string_append"]
       [(equal? exp "#<subr string-length>")            "subr_string_length"]
       [(equal? exp "#<subr string-ref>")               "subr_string_ref"]
       [(equal? exp "#<subr string=?>")                 "subr_string_is_equal"]
       [(equal? exp "#<subr string?>")                  "subr_is_string"]
       [(equal? exp "#<subr substring>")                "subr_substring"]
       [(equal? exp "#<subr symbol->string>")           "subr_symbol2string"]
       [(equal? exp "#<subr symbol?>")                  "subr_is_symbol"]
       [(equal? exp "#<subr vector-length>")            "subr_vector_length"]
       [(equal? exp "#<subr vector-ref>")               "subr_vector_ref"]
       [(equal? exp "#<subr vector-set!>")              "subr_vector_set"]
       [(equal? exp "#<subr list-ref>")                 "subr_list_ref"]
;;       [(equal? exp "#<subr write>")                    "subr_write"]
       [(equal? exp "#<subr write>")                    "(void*)SCH_NIL"]
       [(equal? exp "#<closure open-input-file>")       "subr_open_input_file"]
       [(equal? exp "#<closure open-output-file>")      "subr_open_output_file"]
       [(equal? exp "#<subr close-input-port>")         "subr_close_input_port"]
       [(equal? exp "#<subr close-output-port>")        "subr_close_output_port"]
       [(equal? exp "#<subr current-input-port>")       "subr_current_input_port"]
       [(equal? exp "#<subr current-output-port>")      "subr_current_output_port"]
       [else (string-append "UNKNOWN SUBR[" exp "]")]
       ))))
(define immediate-exp
  (lambda(x)
    (cond [(number? x) (format #f "INT2FIX(~A)" x)]
          [(eqv? x '#t)
           "SCH_TRUE"]
          [(eqv? x '#f)
           "SCH_FALSE"]
          [(symbol? x)
           (format #f "SCH_SYMBOL(\"~A\")" x)]
          [(null? x)
           "SCH_NIL"]
          [(string? x)
           (format #f "SCH_STRING(\"~A\")" x)]
          [(and (list? x)
                (>= 16 (length x)))
           (string-append "SCH_LIST"
                          (number->string (length x))
                          "(" (apply string-append (join (map immediate-exp x) ", ")) ")")]
          [(list? x)
           (string-append (apply string-append
                                 (map (lambda(e)
                                        (string-append "SCH_CONS("
                                                       (immediate-exp e)
                                                       ", "))
                                      x))
                          " SCH_NIL"
                          (format #f "~v,,,')A" (length x) ")"))]
          [(pair? x)
           (let loop ([lst x]
                      [str ""]
                      [count 0])
             (if (pair? lst)
                 (loop (cdr lst)
                       (string-append str
                                      "SCH_CONS("
                                      (immediate-exp (car lst))
                                        ;(symbol->string (car lst))
                                      ", ")
                       (+ count 1))
                 (string-append str
                                (immediate-exp lst)
                                        ;(symbol->string lst)
                                (format #f "~v,,,')A" count ")"))))]
          [(procedure? x)
           (immediate-subr (format #f "~A" x))]
          [(vector? x)
           (format #f "SCH_VECTOR()/*:~S*/" x)]
          [else
           (format #f "SCH_UNDEFINE/*:~S*/" x)])))
(define form-impl
  (lambda(x)
    (cond [(null? x) ""]
          [(pair? x)
           (if (eqv? 'const (car x))
               (string-append "CONST, "
                              (immediate-exp (cadr x))
                              ", "
                              (form-impl (cddr x)))
               (string-append (form-impl (car x))
                              (form-impl (cdr x))))]
          [else (string-upcase
                 (string-append
                  (cond [(symbol? x)
                         (symbol->string x)]
                        [(number? x)
                         (number->string x)]
                        [(eqv? #t x) "SCH_TRUE"]
                        [(eqv? #f x) "SCH_FALSE"]
                        )
                  ", "))])))

(define to_array
  (lambda(fname beg end)
    (define subrs_to_array
      (lambda(vec)
        (let loop ([i 3]
                   [l (vector-length vec)]
                   [str ""])
          (if (< i l)
              (loop (+ i 1)
                    l
                    (string-append str
                                   (if (not (procedure? (vector-ref vec i)))
                                       (string-append "(void*)" (immediate-exp (vector-ref vec i)))
                                       (immediate-exp (vector-ref vec i)))
                                   ", "))
              str))))
    (define subrs_to_names_array
      (lambda(vec)
        (let loop ([i 3]
                   [l (vector-length vec)]
                   [str ""])
          (if (< i l)
              (loop (+ i 1)
                    l
                    (string-append str
                                   "\""
                                   (if (procedure? (vector-ref vec i))
                                       (let* ([subr_str (format #f "~A" (vector-ref vec i)) ]
                                              [len (string-length subr_str)])
                                         (if (char=? (string-ref subr_str 2) #\c)
                                             (substring subr_str 10 (- len 1))
                                             (substring subr_str 7  (- len 1))))
                                       "????")
                                   "\""
                                    ", "))
              str))))
    (define ignore-list
      '(VM
        stack
        push
        index
        index-set!
        closure
        closure-body
        closure-type
        closure-argnum
        index-closure
        *heap*
        *heap-pnt*
        box
        unbox
        set-box!
        continuation
        restore-stack
        *globals-v*
        assign-global!
        index-global
        show-gl
        *builtin-fn*
        ))
    (use srfi-13)
    (call-with-output-file fname
      (lambda(p)
        (let loop ([i beg]
                   [l end]
                   )
          (if (< i l)
              (let ([name (rnth i *globals-n*)]
                    [body (vector-ref *globals-v* i)])
                (if (not (memv name ignore-list))
                    (if (and (vector? body)
                             (or (eqv? 'close0 (vector-ref body 0))
                                 (eqv? 'close1 (vector-ref body 0))
                                 (eqv? 'close2 (vector-ref body 0))))
                        (format p
                                "/* ~A */~%/* ~A */~%SchObj code~A[]   = {~A};~%SubrPnt subrs~A[] = {~A};~%char* subr_names~A[] = {~A};~%globals[~A] = (SchObj)closure2(~A,~A,code~A,~A,subrs~A,subr_names~A);~%~%"
                                name
                                body
                                i
                                (form-impl (vector-ref body 2))
                                i
                                (subrs_to_array body)
                                i
                                (subrs_to_names_array body)
                                i
                                (string-upcase (symbol->string (vector-ref body 0)))
                                (length (vector-ref body 2))
                                i
                                (- (vector-length body) 3)
                                i
                                i)
                        (if (eqv? name '*globals-n*)
                            (format p "/* ~A */~%globals[~A] = ~A;~%~%" name i (immediate-exp *globals-n*))
                            (format p "/* ~A */~%globals[~A] = ~A;~%~%" name i (immediate-exp body)))
                        ))

                (cond [(eqv? name 'p1)
                       (format p "#define INDEX_P1 ~A~%~%" i)]
                      [(eqv? name '*globals-n*)
                       (format p "#define INDEX_GLOBALS_N ~A~%~%" i)])
                (loop (+ i 1)
                      l))))))))


(to_array "./compile.c" 0 (length *globals-n*))
(exit)
