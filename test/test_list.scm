(run-test "test_list.scm:list1 "
          (begin
            [assert (pair? '(8 9)) #t]
            [assert (pair? 7) #f]
            [assert (pair? '(a b))  #t]
            [assert (pair? '(a b c)) #t]
            [assert (pair? '()) #f]
            [assert (pair? '#(a b)) #f]
            [assert (car (cons 5 6)) 5]
            [assert (car (cons 1 2)) 1]
            [assert (cdr (cons 1 2)) 2]
            [assert (let ((kons (cons 3 4)))(set-car! kons 9)(car kons)) 9]
            [assert (let ((kons (cons 3 4)))(set-cdr! kons 9)(cdr kons)) 9]

            [assert (null? '()) #t]
            [assert (null? 6) #f]

            [assert (car (list 5)) 5]
            [assert (cdr (list 5)) '()]
            [assert (car  (list 4 5)) 4]
            [assert (cadr (list 4 5)) 5]
            [assert (cddr (list 4 5)) '()]
            [assert (car    '(3 4 (5 6))) 3]
            [assert (cadr   '(3 4 (5 6))) 4]
            [assert (caaddr '(3 4 (5 6))) 5]
            [assert (car (cdaddr '(3 4 (5 6)))) 6]
            [assert (cdddr   '(3 4 (5 6))) '()]

            [assert (length '(9)) 1]
            [assert (length '(4 5)) 2]
            [assert (length '(4 5 (6 7))) 3]
            [assert (length '()) 0]

            [assert (list? '(1 2)) #t]
            [assert (list? '()) #f]
            [assert (list? (cons 1 2)) #f]

            [assert (append '(x) '(y))        '(x y)]
            [assert (append '(a) '(b c d))    '(a b c d)]
            [assert (append '(a (b)) '((c)))  '(a (b) (c))]
            [assert (append '(a b) '(c . d))  '(a b c . d)]
            [assert (append '() 'a)           'a]
            [assert (append '(a) '(b) '(c))   '(a b c)]

            [assert (let* ((hd  '(x))
                           (cmp (append hd '(y))))
                      (eq? hd cmp)) #f]

            [assert (memq 'a '(a b c)) '(a b c)]
            [assert (memq 'b '(a b c)) '(b c)]
            [assert (memq 'a '(b c d)) #f]
            [assert (memq (list 'a) '(b (a) c)) #f]

            [assert (memv 101 '(100 101 102)) '(101 102)]

            [assert (assq 'a '((a 1) (b 2) (c 3))) '(a 1)]
            [assert (assq 'b '((a 1) (b 2) (c 3))) '(b 2)]
            [assert (assq 'd '((a 1) (b 2) (c 3))) #f]

            [assert (assq (list 'a) '(((a)) ((b)) ((c)))) #f]

;; /* (assq 5 '((2 3) (5 7) (11 13)))         => unspecified */
;; /*     ARG2( INT2FIX(5), SCH_LIST3( SCH_LIST2( INT2FIX(2), INT2FIX(3)  ), */
;; /*                                  SCH_LIST2( INT2FIX(5), INT2FIX(7)  ), */
;; /*                                  SCH_LIST2( INT2FIX(11),INT2FIX(13) ) ) ); */

            [assert (member (list 'a) '(b (a) c)) '((a) c)]
            [assert (assv 5 '((2 3) (5 7) (11 13))) '(5 7)]
            [assert (assoc (list 'a) '( ((a)) ((b)) ((c)) )) '((a))]

            ))

(run-test "test_list.scm:list2 "
          (begin
            [assert (caar '((77))) 77]
            [assert (cadr '(99 88)) 88]
            [assert (cdar '((66 . 77)))  77]
            [assert (cddr '(22 33 . 44))  44]
            [assert (cadar '((44 55))) 55]
            [assert (caddr '(33 44 55)) 55]
            [assert (cdddr '(66 77 88 99)) '(99)]
            [assert (cadddr '(66 77 88 99)) 99]
            [assert (cddddr '(55 66 77 88 99)) '(99)]
            [assert (caaaar '((((a))))) 'a]
            [assert (caaadr '(b ((a)))) 'a]
            [assert (caaar '(((a)))) 'a]
            [assert (caadar '((c (a)))) 'a]
            [assert (caaddr '(c b (a))) 'a]
            [assert (caadr '(b (a))) 'a]
            [assert (cadaar '(((b a))) ) 'a]
            [assert (cadadr '(c (b a))) 'a]
            [assert (caddar '((c b a)) )  'a]
            [assert (cdaaar '((((b . a)))) )  'a]
            [assert (cdaadr '(c ((b . a))) )  'a]
            [assert (cdaar '(((b . a))) )  'a]
            [assert (cdadar '((c (b . a))) )  'a]
            [assert (cdaddr '(d c (b . a)) )  'a]
            [assert (cdadr '(c (b . a)) )  'a]
            [assert (cddaar '(((c b . a))) )  'a]
            [assert (cddadr '(d (c b . a)) )  'a]
            [assert (cddar '((c b . a)) )  'a]
            [assert (cdddar '((d c b . a)) )  'a]

            [assert (list-ref '(d e f) 1)  'e]
            [assert (list-ref '(a b c) 1)  'b]
            [assert (list-ref '(d e f) 0)  'd]
            [assert (list-ref '(d e f) 1)  'e]
            [assert (list-ref '(d e f) 2)  'f]
            [assert (list-tail '(a b c) 1)  '(b c)]
            [assert (reverse '(a b c))  '(c b a)]

            ))

