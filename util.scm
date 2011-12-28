;;; -----------------------------------------------------------------
;;; util
;;; -----------------------------------------------------------------

(define *undefined* (undefined))

;; set -----------------------------------
(define set-member?
  (lambda (x s)
    (cond
     [(null? s) #f]
     [(eq? x (car s)) #t]
     [else (set-member? x (cdr s))])))

(define set-cons
  (lambda (x s)
    (if (set-member? x s)
        s
        (cons x s))))

(define set-union
  (lambda (s1 s2)
    (if (null? s1)
        s2
        (set-union (cdr s1) (set-cons (car s1) s2)))))

(define set-minus
  (lambda (s1 s2)
    (if (null? s1)
        '()
        (if (set-member? (car s1) s2)
            (set-minus (cdr s1) s2)
            (cons (car s1) (set-minus (cdr s1) s2))))))

(define set-intersect
  (lambda (s1 s2)
    (if (null? s1)
        '()
        (if (set-member? (car s1) s2)
            (cons (car s1) (set-intersect (cdr s1) s2))
            (set-intersect (cdr s1) s2)))))

;; list ------------------------------------
(define flat-length
  (lambda(l)
    (if (null? l)
        0
        (if (pair? l)
            (+ (flat-length (car l))
               (flat-length (cdr l)))
            1 ))))

(define flatten
  (lambda(lst)
    (cond [(pair? lst)
           (append (flatten (car lst))
                   (flatten (cdr lst)))]
          [(null? lst)
           '()]
          [else
           (cons lst '())])))

;  e.g  (replace '(a b) '((a . z)(b . x))) => (z x)
(define replace
  (lambda(lst al)
    (cond [(pair? lst)
           (cons (replace (car lst) al)
                 (replace (cdr lst) al))]
          [(and (symbol? lst)
                (assq lst al))
           (cdr (assq lst al))]
          [else lst])))

(define nth
  (lambda(n lst)
    (if (= n 0)
        (car lst)
        (nth (- n 1)
             (cdr lst)))))

(define rnth
  (lambda(n lst)
    (nth (- (length lst) n 1) lst)))

(define ncdr
  (lambda(n lst)
    (if (= n 0)
        lst
        (ncdr (- n 1)
             (cdr lst)))))

(define join
  (lambda(lst x)
    (define join0
      (lambda(lst x)
      (if (null? (cdr lst))
          (cons x lst)
          (cons x (cons (car lst)
                        (join0 (cdr lst) x))))))
    (if (and (pair? lst)
             (pair? (cdr lst)))
        (cons (car lst)
              (join0 (cdr lst) x))
        lst)))

(define caddddr
  (lambda(lst)
    (car (cddddr lst))))
;; string util ------------------------------
(define replace-str
  (lambda(str old new)
    (define replace-str-impl
      (lambda(before after old new)
        (if (< 0 (string-length after))
            (replace-str-impl (string-append before
                                             (make-string 1
                                                          (if (char=? (string-ref after 0)
                                                                      old)
                                                              new
                                                              (string-ref after 0)))
                                             )
                              (substring after 1 (string-length after))
                              old
                              new)
            before)))
    (replace-str-impl "" str old new)))


;; for debug --------------------------------
(define *debug-flag* #f)
(define *output-globals* #f)
(define *debug-out-port* #t)
(define *show-stack* #t)
(define *show-reg-c* #t)

(define show-reg
  (lambda(a x f c s pc)
;    (trace #t "~%   [a] --- ~A~%   [x] --- ~A~%   [f] --- ~A~%   [c] --- ~A~%   [s] --- " a x f (if *show-reg-c* c ""))
    (trace #t "~%   [a] --- ~A~%   [x] --- ~A~%   [p] --- ~A~%   [f] --- ~A~%   [c] --- ~A~%   [s] --- " a x pc f (if *show-reg-c* c ""))
    (if *show-stack*
        (ins-st s)
        #f)
    ))

;(define show-reg
;  (lambda(a pc f c s)
;    (trace #t
;           "~%   [a] --- ~A~%   [x] --- ~A ~A ~A ~A ~A ~A~%   [f] --- ~A~%   [c] --- ~A~%   [s] --- "
;           a
;           (ins pc) (ins (+ pc 1)) (ins (+ pc 2)) (ins (+ pc 3)) (ins (+ pc 4)) (ins (+ pc 5))
;           f
;           (if *show-reg-c* c ""))
;    (if *show-stack*
;        (ins-st s)
;        #f)
;    ))

(define ins-st
  (lambda (p)
    (trace #t "=> ~3@A [            ]~%" p)
    (let loop ((i p))
;      (unless (< i 1)
      (unless (or (< i 1)(< (+ i 1) p))
        (trace #t "                ~A [ ~10@A ]~%" (- i 1) (index p (- p i)))
        (loop (- i 1)))
      )))

(define trace
  (lambda(opt frmt . args)
    (if *debug-flag*
        (apply format *debug-out-port* frmt args))))

(define output-globals
  (lambda()
    (if *output-globals*
        (let loop ([i 0]
                   [l (vector-length *globals-v*)])
          (if (< i l)
              (begin (call-with-output-file
                         (string-append "./work/"
                                        (number->string (+ 1000 i))
                                        "_"
                                        (if (< i (length *globals-n*))
                                            (let ([name (rnth i *globals-n*)])
                                              (if (symbol? name)
                                                  (replace-str (replace-str (symbol->string name) #\* #\A) #\? #\P)
                                                  "unknown"))
                                            "undef")
                                        ".scm")
                       (lambda(p)
                         (write (vector-ref *globals-v* i) p)))
                     (loop (+ i 1) l))
              #f)
          )
        #f)))
