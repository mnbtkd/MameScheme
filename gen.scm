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


;; Map a built-in procedure name (symbol) to the C identifier of its
;; implementation in the runtime.  Keyed on the Scheme *name* rather than
;; on the procedure's printed representation, so it does not depend on how
;; Gauche happens to print subrs/closures (that format has changed across
;; versions, e.g. "#<subr cdr>" vs. "#<subr (cdr obj)>").
(define *subr-name->cname*
  '((+                   . "subr_add")
    (-                   . "subr_sub")
    (<=                  . "subr_is_equal_to_or_greater")
    (<                   . "subr_is_greater")
    (=                   . "subr_is_equal_to_or_less")
    (append              . "subr_append")
    (apply               . "subr_apply")
    (assq                . "subr_assq")
    (boolean?            . "subr_is_boolean")
    (caar                . "subr_caar")
    (cadr                . "subr_cadr")
    (car                 . "subr_car")
    (cdar                . "subr_cdar")
    (cddr                . "subr_cddr")
    (cdr                 . "subr_cdr")
    (char=?              . "subr_char_is_equal")
    (char?               . "subr_is_char")
    (cons                . "subr_cons")
    (eq?                 . "subr_is_eq")
    (equal?              . "subr_is_equal")
    (eqv?                . "subr_is_eqv")
    (length              . "subr_length")
    (list                . "subr_list")
    (list?               . "subr_is_list")
    (make-string         . "subr_make_string_k")
    (make-vector         . "subr_make_vector")
    (map                 . "subr_map")
    (memq                . "subr_memq")
    (memv                . "subr_memv")
    (not                 . "subr_not")
    (null?               . "subr_is_null")
    (number->string      . "subr_number2string")
    (number?             . "subr_is_number")
    (pair?               . "subr_is_pair")
    (procedure?          . "subr_is_procedure")
    (set-cdr!            . "subr_set_cdr")
    (string->symbol      . "subr_string2symbol")
    (string-append       . "subr_string_append")
    (string-length       . "subr_string_length")
    (string-ref          . "subr_string_ref")
    (string=?            . "subr_string_is_equal")
    (string?             . "subr_is_string")
    (substring           . "subr_substring")
    (symbol->string      . "subr_symbol2string")
    (symbol?             . "subr_is_symbol")
    (vector-length       . "subr_vector_length")
    (vector-ref          . "subr_vector_ref")
    (vector-set!         . "subr_vector_set")
    (list-ref            . "subr_list_ref")
    (cdadr               . "subr_cdadr")
    (caadr               . "subr_caadr")
    (cadar               . "subr_cadar")
    (cadddr              . "subr_cadddr")
    (caddr               . "subr_caddr")
    (cddddr              . "subr_cddddr")
    (cdddr               . "subr_cdddr")
    (open-input-file     . "subr_open_input_file")
    (open-output-file    . "subr_open_output_file")
    (close-input-port    . "subr_close_input_port")
    (close-output-port   . "subr_close_output_port")
    (current-input-port  . "subr_current_input_port")
    (current-output-port . "subr_current_output_port")
    ;; emitted as a NIL placeholder rather than a real subr pointer
    (write               . "(void*)SCH_NIL")
    (format              . "(void*)SCH_NIL")))

;; Resolve an embedded built-in procedure object to its Scheme name by
;; object identity, using the correspondence eval.scm already maintains
;; between *builtin-fn* (the procedure objects) and *builtin-fn-name*
;; (their names).  This is completely independent of the printed form.
(define *subr-obj->name*
  (let loop ([i 3] [acc '()])
    (if (< i (vector-length *builtin-fn*))
        (loop (+ i 1)
              (cons (cons (vector-ref *builtin-fn* i)
                          (list-ref *builtin-fn-name* (- i 3)))
                    acc))
        acc)))

(define subr-name
  (lambda(x)
    (cond [(assq x *subr-obj->name*) => cdr]
          [else #f])))

(define immediate-subr
  (lambda(x)
    (let ([name (subr-name x)])
      (cond [(and name (assq name *subr-name->cname*)) => cdr]
            [else (string-append "UNKNOWN SUBR[" (format #f "~A" x) "]")]))))
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
           (immediate-subr x)]
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
                                   (let ([name (subr-name (vector-ref vec i))])
                                     (if name (symbol->string name) "????"))
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
