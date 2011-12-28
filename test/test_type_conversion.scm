(run-test "test_type_conversion.scm:type_conversion "
           (begin
             [assert (symbol->string 'abc) "abc"]
             [assert (string->symbol "abc") 'abc]
             [assert (char->integer #\a)  97]
             [assert (char->integer #\A)  65]
             [assert (char->integer #\9)  57]
             [assert (integer->char 97) #\a]
             [assert (integer->char 65) #\A]
             [assert (integer->char 57) #\9]

             [assert (number->string 99) "99"]
             [assert (number->string 99 10) "99"]
             [assert (number->string 99  8) "143"]
             [assert (number->string 99 16) "63"]
             [assert (number->string 99 24) "99"]

             [assert (list->vector '(1 2 3)) #(1 2 3)]
             [assert (list->vector '(a (1 2 3) b c)) #(a (1 2 3) b c)]
             ))

