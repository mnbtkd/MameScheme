;;; ------------------------------------------------
;;; VM
;;; ------------------------------------------------
;;; a --- accumulator register
;;; x --- instructions
;;; f --- frame
;;; c --- closure
;;; s --- stack pointer

;;; pc --- program counter

;(define ins
;  (lambda(pc)
;    (if (< pc (vector-length *instructions*))
;        (vector-ref *instructions* pc)
;        'OUT-OF-RANGE)))
(define VM
  (lambda (a x f c s pc)

    (define list-args
      (lambda(s n)
        (let loop ([n (- n 1)]
                   [a '()])
          (if (< n 0)
              a
              (loop (- n 1)
                    (cons (index s n) a))))))

    (define list-args2
      (lambda(s x n)
        (let loop ([n (- n 1)]
                   [x (- x 1)]
                   [a '()])
          (if (< n 0)
              a
              (loop (- n 1)
                    (- x 1)
                    (cons (index s x)
                          a))))))

    ;; スタックのトップからn個をm個分下に下げて、スタックトップの位置を返す
    (define shift-args
      (lambda (n m s)
        (let nxtarg ([i (- n 1)])
          (unless (< i 0)
            (index-set! s (+ i m) (index s i))
            (nxtarg (- i 1))))
        (- s m)))


;    (define 1+ (lambda(n)(+ 1 n)))
;    (define 2+ (lambda(n)(+ 2 n)))
;    (define 3+ (lambda(n)(+ 3 n)))
;    (define 4+ (lambda(n)(+ 4 n)))
;    (define 5+ (lambda(n)(+ 5 n)))

;    (trace #t "~%~A" (string-upcase (symbol->string (nth 0 x))))
    (trace #t "~% ----- insn[~A] -------------------------~%" (nth 0 x))
    (show-reg a x f c s pc)

    ;(show-gl)
    (let ([key (nth 0 x)])
      (case key
        ['halt         a]
        ['lref         (VM (index f (nth 1 x))         (cddr x) f c s (+ 1 pc))]
        ['fref         (VM (index-closure c (nth 1 x)) (cddr x) f c s (+ 1 pc))]
        ['gref         (VM (index-global (nth 1 x))    (cddr x) f c s (+ 1 pc))]
        ['unbox        (VM (unbox a)                   (cdr  x) f c s (+ 1 pc))]
        ['const        (VM (nth 1 x)                   (cddr x) f c s (+ 1 pc))]
        ['push         (VM a                           (cdr  x) f c (push a s) (+ 1 pc))]
        ['frame        (VM a                           (cddr x) f c (push (ncdr (+ 2 (nth 1 x)) x) (push f (push c s))) (+ 1 pc))]
        ['return       (let ([s (- s (nth 1 x))])
                         (VM a (index s 0) (index s 1) (index s 2) (- s 3) (+ 1 pc)))]
        ['call
         (if (procedure? a)
             (VM (let loop ([i (- (nth 1 x) 1)]
                            [lst '()])
                   (if (< i 0)
                       (apply a lst)
                       (loop (- i 1) (cons (index s i) lst))))
                 (ncdr 2 x)
                 f c s (+ 1 pc))
             (let ([type (closure-type a)])
               (case type
                 ['close1
                  (let ([diff (- (nth 1 x) 1)])
                    (index-set! s
                                diff
                                (list-args s (nth 1 x)))
                    (VM a (closure-body a) (- s diff) a (- s diff) (+ 1 pc)))]
                 ['close2
                  (let ([diff (- (nth 1 x)
                                 (closure-argnum a))])
                    (index-set! s
                                (- (nth 1 x) 1)
                                (list-args2 s
                                            (nth 1 x)
                                            (+ diff 1)))
                    ; 上から数えて (- (closure-argnum a) 1) 個分の引数を (- (nth 1 x) (closure-argnum a)) 個さげればいい。
                    (let ([st (shift-args (- (closure-argnum a) 1)
                                          diff
                                          s)])
                      (VM a (closure-body a) st a st (+ 1 pc))))
                  ]
                 ['close0
                  (VM a (closure-body a) s a s (+ 1 pc))]
               ))
             )]
        ['test         (VM a (if a (ncdr 2 x) (ncdr (+ 2 (nth 1 x)) x)) f c s (+ 1 pc))]
        ['jump         (VM a (ncdr (+ 2 (nth 1 x)) x) f c s (+ 1 pc))]
        ['shift        (VM a (ncdr 3 x) f c (shift-args (nth 1 x) (nth 2 x) s) (+ 1 pc))]
                       ;;           type    argnum    body       n         s
        ['close0       (VM (closure 'close0 (nth 2 x) (ncdr 4 x) (nth 1 x) s) (ncdr (+ 4 (nth 3 x)) x) f c (- s (nth 1 x)) (+ 1 pc))]
        ['close1       (VM (closure 'close1 (nth 2 x) (ncdr 4 x) (nth 1 x) s) (ncdr (+ 4 (nth 3 x)) x) f c (- s (nth 1 x)) (+ 1 pc))]
        ['close2       (VM (closure 'close2 (nth 2 x) (ncdr 4 x) (nth 1 x) s) (ncdr (+ 4 (nth 3 x)) x) f c (- s (nth 1 x)) (+ 1 pc))]

        ['box          (index-set! s (nth 1 x) (box (index s (nth 1 x))))
                       (VM a (ncdr 2 x) f c s (+ 1 pc))]
        ['lset         (set-box! (index f (nth 1 x)) a)
                       (VM a (ncdr 2 x) f c s (+ 1 pc))]
        ['gset         (VM (assign-global! (nth 1 x) a) (ncdr 2 x) f c s (+ 1 pc))]



        ['conti        (VM (continuation s) (ncdr 1 x) f c s (+ 1 pc))]
        ['nuate        (VM a (ncdr 2 x) f c (restore-stack (nth 1 x)) (+ 1 pc))]


        ['fset  (set-box! (index-closure c (nth 1 x)) a)
                       (VM a (ncdr 2 x) f c s (+ 1 pc))]
        
        ))))


;;;; -----------------------------------------
;;;; stack
;;;; -----------------------------------------
(define stack (make-vector 2000)) ; TODO hard code

(define push
  (lambda (x s)
    (vector-set! stack s x)
    (+ s 1)))

(define index
  (lambda (s i)
    (vector-ref stack (- s i 1))))

(define index-set!
  (lambda (s i v)
    (vector-set! stack (- s i 1) v)))


;;;; -----------------------------------------
;;;; closure
;;;; -----------------------------------------
(define closure
  (lambda (type argnum body n s)
    (let ([v (make-vector (+ n 3))])
      (vector-set! v 0 type)
      (vector-set! v 1 argnum)
      (vector-set! v 2 body)
      (let f ([i 0])
        (unless (= i n)
          (vector-set! v (+ i 3) (index s i))
          (f (+ i 1))))
      v)))

(define closure-body
  (lambda (c)
    (vector-ref c 2)))

(define closure-type
  (lambda (c)
    (vector-ref c 0)))

(define closure-argnum
  (lambda (c)
    (vector-ref c 1)))

(define index-closure
   (lambda (c n)
      (vector-ref c (+ n 3))))



;;;; -----------------------------------------
;;;; box
;;;; -----------------------------------------
(define *heap* (make-vector 150000))
(define *heap-pnt* 99900)
(define box
  (lambda (x)
    (vector-set! *heap* (- *heap-pnt* 99900) x)
    (set! *heap-pnt* (+ 1 *heap-pnt*))
    (- *heap-pnt* 1)))
(define unbox
  (lambda (i)
    (vector-ref *heap* (- i 99900))))
(define set-box!
  (lambda(p x)
    (vector-set! *heap* (- p 99900) x)))

;;;; -----------------------------------------
;;;; continuation
;;;; -----------------------------------------
(define continuation
  (lambda (s)
    (define save-stack
      (lambda (s)
        (let ([v (make-vector s)])
          (let copy ([i 0])
            (unless (= i s)
              (vector-set! v i (vector-ref stack i))
              (copy (+ i 1))))
          v)))
    (closure 'close0
             -1
;;             (list 'lref 0 (list 'nuate (save-stack s) '(return 0)))
             (list 'lref 0 'nuate (save-stack s) 'return 0)
             0 0)))
;(define closure
;  (lambda (type argnum body n s)

(define restore-stack
  (lambda (v)
    (let ([s (vector-length v)])
      (let copy ([i 0])
        (unless (= i s)
          (vector-set! stack i (vector-ref v i))
          (copy (+ i 1))))
      s)))

;;;; -----------------------------------------
;;;; global
;;;; -----------------------------------------
(define *globals-v* (make-vector 200))
(define assign-global!
  (lambda(i x)
    (vector-set! *globals-v* i x)))
(define index-global
  (lambda(i)
    (vector-ref *globals-v* i)))

(define show-gl
  (lambda()
    (trace #t "~%   [g] --- ~A~%" *globals-v*)))

