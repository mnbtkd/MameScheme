;;; -----------------------------------------------------------------
;;; library syntax
;;; -----------------------------------------------------------------

(run '(define-syntax when
        (syntax-rules ()
          ((_ pred b1 ...)
           (if pred (begin b1 ...))))))

(run '(define-syntax unless
        (syntax-rules ()
          ((_ pred b1 ...)
           (if (not pred)
               (begin
                 b1 ...))))))

(run '(define-syntax or
        (syntax-rules ()
          ((or) #f)
          ((or test) test)
          ((or test1 test2 ...)
           (let ((x test1))
             (if x x (or test2 ...)))))))

(run '(define-syntax and
        (syntax-rules ()
          ((and) #t)
          ((and test) test)
          ((and test1 test2 ...)
           (if test1 (and test2 ...) #f)))))

(run '(define-syntax letrec
        (syntax-rules()
          [(letrec ((fn def) ...) body ...)
           (let ((fn *undefined*) ...)
             (set! fn def) ...
             body ...)])))

(run '(define-syntax let
        (syntax-rules ()
          ((let ([name val] ...) body1 body2 ...)
           ((lambda (name ...)
              (begin
                body1
                body2 ...))
            val ...))
          ((let tag ([name val] ...)
               body1
               body2 ...)
           ((letrec ([tag (lambda (name ...)
                            (begin body1 body2 ...))])
              tag)
            val ...)))))

(run '(define-syntax let*
        (syntax-rules ()
          ((let* () body1 body2 ...)
           (let () body1 body2 ...))
          ((let* ((name1 val1) (name2 val2) ...)
             body1 body2 ...)
           (let ((name1 val1))
             (let* ((name2 val2) ...)
               body1 body2 ...))))))

(run '(define-syntax case
        (syntax-rules (else)
          ((case (key ...)
             clauses ...)
           (let ((atom-key (key ...)))
             (case atom-key clauses ...)))
          ((case key
             (else result1 result2 ...))
           (begin result1 result2 ...))
          ((case key
             ((atoms ...) result1 result2 ...))
           (if (memv key '(atoms ...))
               (begin result1 result2 ...)))
          ((case key
             ((atoms ...) result1 result2 ...)
             clause clauses ...)
           (if (memv key '(atoms ...))
               (begin result1 result2 ...)
               (case key clause clauses ...))))))

(run '(define-syntax cond
        (syntax-rules (else =>)
          ((cond (else result1 result2 ...))
           (begin result1 result2 ...))
          ((cond (test => result))
           (let ((temp test))
             (if temp (result temp))))
          ((cond (test => result) clause1 clause2 ...)
           (let ((temp test))
             (if temp
                 (result temp)
                 (cond clause1 clause2 ...))))
          ((cond (test)) test)
          ((cond (test) clause1 clause2 ...)
           (let ((temp test))
             (if temp
                 temp
                 (cond clause1 clause2 ...))))
          ((cond (test result1 result2 ...))
           (if test (begin result1 result2 ...)))
          ((cond (test result1 result2 ...)
                 clause1 clause2 ...)
           (if test
               (begin result1 result2 ...)
               (cond clause1 clause2 ...))))))

(run '(define-syntax do
        (syntax-rules ()
          ((do ((var init step ...) ...)
               (test expr ...)
             command ...)
           (letrec
               ((loop
                 (lambda (var ...)
                   (if test
                       (begin
                         *undefined*
                         expr ...)
                       (begin
                         command
                         ...
                         (loop (do 'step//// var step ...) ; TODO 変なシンボル使ってるので変更する
                               ...))))))
             (loop init ...)))
          ((do 'step//// x)
           x)
          ((do 'step//// x y)
           y))))

(run '(define-syntax delay
        (syntax-rules ()
          ((delay expression)
           (make-promise (lambda () expression))))))
