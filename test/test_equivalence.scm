(run-test "test_equivalence.scm:equivalence "
          (begin
            [assert (let [(x '(3 . 4))] (eq? x x))                 #t]
            [assert (eq? '(3 . 4) '(3 . 4))                        #f]
            [assert (eqv? #f #f)                                   #t]
            [assert (eqv? #t #t)                                   #t]
            [assert (eqv? 'abc 'abc)                               #t]
            [assert (eqv? 99 99)                                   #t]
            [assert (eqv? #\z #\z)                                 #t]
            [assert (eqv? '() '())                                 #t]
            [assert (eqv? "object" "object")                       #f]
            [assert (eqv? '(1 . 2) '(1 . 2))                       #f]
            [assert (eqv? #f '())                                  #f]
            [assert (eqv? #(99 99 99) #(99 99 99))                 #f]
            [assert (equal? 'a 'a)                                 #t]
            [assert (equal? '(a (b) c) '(a (b) c))                 #t]
            [assert (equal? "abc" "abc")                           #t]
            [assert (equal? 2 2)                                   #t]
            [assert (equal? 0.5 0.5)                               #t]
            [assert (equal? (make-vector 5 'a) (make-vector 5 'a)) #t]
            [assert (equal? #(a (1 2 3) b c) #(a (1 2 3) b c))     #t]
            [assert (equal? #(a (1 2 3) b c) #(a (1 2 4) b c))     #f]
            [assert (equal? #(a (1 2 3) b c) #(a (1 2 4) b d))     #f]

            [assert (< 1 2)                                        #t]
            [assert (< 2 1)                                        #f]
            [assert (< 2 2)                                        #f]
            [assert (< 1 2 3 4)                                    #t]
            [assert (< 1 2 2 3)                                    #f]
            [assert (< -1 0)                                       #t]
            [assert (< -99 -1)                                     #t]

            [assert (> 2 1)                                        #t]
            [assert (> 1 2)                                        #f]
            [assert (> 2 2)                                        #f]
            [assert (> -1 -2)                                      #t]
            [assert (> -1 0)                                       #f]
            [assert (> 3 2 1 0)                                    #t]
            [assert (> 3 2 2 1)                                    #f]
            [assert (> -1 -2 -3 -4)                                #t]

            [assert (<= 3 4)                                       #t]
            [assert (<= 5 5)                                       #t]
            [assert (<= 7 6)                                       #f]
            [assert (<= 1 2 2 3)                                   #t]
            [assert (<= 3 2 2 1)                                   #f]
            [assert (<= -1 -1)                                     #t]
            [assert (<= -1 0)                                      #t]
            [assert (<= 0 -1)                                      #f]

            [assert (>= 4 3)                                       #t]
            [assert (>= 5 5)                                       #t]
            [assert (>= 6 7)                                       #f]
            [assert (>= -1 0)                                      #f]
            [assert (>= 0 -1)                                      #t]
            [assert (>= -1 -1)                                     #t]
            [assert (>= 3 2 2 1)                                   #t]
            [assert (>= 1 2 2 3)                                   #f]
            [assert (>= 0 -1 -1 -2)                                #t]





            ))


(run-test "test_equivalence.scm:type_equivalence "
          (begin
            [assert (integer? #\a)   #f]
            [assert (integer? #t)    #f]
            [assert (integer? #f)    #f]
            [assert (integer? '())   #f]
            [assert (integer? 6)     #t]
            [assert (integer? '(6))  #f]
            [assert (integer? #(6))  #f]
            [assert (integer? 6.1)   #f]
            [assert (integer? -1)    #t]
            [assert (integer? 3.5)   #f]
            [assert (integer? 3.0)   #t]
            [assert (integer? 3.00000000000001)   #f]
            [assert (integer? 3.00000000000000)   #t]
            [assert (integer? 8/4)   #t]
            [assert (integer? 12345678987654321123456789876543211234567898765432112345678987654321) #t]
            [assert (integer? 4/1)   #t]
            ;; [assert (integer? 3+0i) #t]

            [assert (boolean? #t)   #t]
            [assert (boolean? 6)    #f]
            [assert (boolean? '())  #f]
            [assert (boolean? #(#t))  #f]
            [assert (boolean? '(#t))  #f]
            [assert (boolean? "#t")   #f]

            [assert (symbol? 'abc)  #t]
            [assert (symbol? 8)  #f]
            [assert (symbol? '())  #f]
            [assert (symbol? "xyz")  #f]
            [assert (symbol? #f)  #f]

            [assert (number? '())  #f]
            [assert (number? '(99))  #f]
            [assert (number? "99")   #f]
            [assert (number? 99)   #t]
            [assert (number? -99)  #t]
            [assert (number? 3.1)  #t]
            [assert (number? -3.1)  #t]
            [assert (number? 1/2)   #t]
            [assert (number? -1/2)  #t]

            [assert (procedure? 1)    #f]
            [assert (procedure? '())  #f]
            [assert (procedure? number?)  #t]
            [assert (procedure? (lambda()'()))  #t]

))


(run-test "test_equivalence.scm:number_equivalence "
          (begin
            [assert (= 3 3)     #t]
            [assert (= 0 0)     #t]
            [assert (= 4 5)     #f]
            [assert (= 5 4)     #f]
            [assert (= 2 2 2)   #t]
            [assert (= 2 3 4)   #f]
            [assert (= -1 -1)   #t]
            [assert (= -1 0)    #f]
            [assert (= 1/2 0.5) #t]
            [assert (= 1 1.0)   #t]

            [assert (= 1/3 0.3333333333333333333333333333333333333)  #t]
            [assert (= 8147239171756556437613610622889275333631069052587756 8147239171756556437613610622889275333631069052587756)  #t]
            [assert (= 9133761082225474145008842305312577714790766296039436 -9133761082225474145008842305312577714790766296039436)  #f]
            [assert (= -2784982954331647429507618610197585909816121841901447 -2784982954331647429507618610197585909816121841901447)  #t]
            [assert (= 8147236941929944541466268634067058317375364585977415464659622695317987026070500394706743
                8147236941929944541466268634067058317375364585977415464659622695317987026070501394706743)  #f]
))

