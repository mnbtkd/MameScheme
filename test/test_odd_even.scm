(begin
  (define *title*  "t.scm")
  (define *passed* '())
  (define *failed* '())
  (run-test "odd_even"
            [(even? 2)  . #t]
            [(odd?  3)  . #t]
            [(even? 3)  . #f]
            [(odd?  4)  . #f]
            [(even? -2) . #t]
            [(odd?  -3) . #t]
            [(even? -3) . #f]
            [(odd?  -4) . #f]
            [(even? 0)  . #t]
            [(odd?  0)  . #f]
            ))

