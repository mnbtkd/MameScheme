(run-test "test_char.scm:char "
          (begin
            [assert (char? #\a)  #t]
            [assert (char? 9)    #f]
            [assert (char? '())  #f]
            [assert (char? #f)   #f]
            [assert (char? '(#\a))  #f]
            [assert (char? #(#\a))  #f]
            [assert (char? "abc")  #f]
            [assert (char? "a")  #f]

            [assert (char=? #\a #\a)  #t]
            [assert (char=? #\a #\z)  #f]
            [assert (char=? #\a #\A)  #f]
            [assert (char=? #\a "a")  #f]

            [assert (char-ci=? #\a #\a )  #t]
            [assert (char-ci=? #\a #\z )  #f]
            [assert (char-ci=? #\a #\A )  #t]
            [assert (char-ci=? #\Z #\z )  #t]
            [assert (char-ci=? #\A "A" )  #f]

            [assert (char<? #\a #\b)  #t]
            [assert (char<? #\A #\B)  #t]

            [assert (char<? #\a #\A)  #f]
            [assert (char<? #\9 #\a)  #t]
            [assert (char<? #\9 #\A)  #t]
            [assert (char<? #\a #\a)  #f]
            [assert (char<? #\A #\a)  #t]

            [assert (char-ci<? #\a #\b)  #t]
            [assert (char-ci<? #\a #\B)  #t]
            [assert (char-ci<? #\A #\b)  #t]
            [assert (char-ci<? #\a #\A)  #f]
            [assert (char-ci<? #\9 #\a)  #t]
            [assert (char-ci<? #\9 #\A)  #t]
            [assert (char-ci<? #\a #\a)  #f]

            [assert (char>? #\b #\a)  #t]
            [assert (char>? #\B #\A)  #t]
            [assert (char>? #\A #\a)  #f]
            [assert (char>? #\a #\9)  #t]
            [assert (char>? #\A #\9)  #t]
            [assert (char>? #\a #\a)  #f]

            [assert (char-ci>? #\b #\a)  #t]
            [assert (char-ci>? #\B #\A)  #t]
            [assert (char-ci>? #\b #\A)  #t]
            [assert (char-ci>? #\B #\a)  #t]
            [assert (char-ci>? #\A #\a)  #f]
            [assert (char-ci>? #\a #\9)  #t]
            [assert (char-ci>? #\A #\9)  #t]
            [assert (char-ci>? #\a #\a)  #f]

            [assert (char<=? #\a #\b)  #t]
            [assert (char<=? #\a #\a)  #t]
            [assert (char<=? #\b #\a)  #f]
            [assert (char<=? #\9 #\a)  #t]
            [assert (char<=? #\9 #\A)  #t]

            [assert (char-ci<=? #\a #\b)  #t]
            [assert (char-ci<=? #\a #\a)  #t]
            [assert (char-ci<=? #\a #\A)  #t]
            [assert (char-ci<=? #\b #\a)  #f]
            [assert (char-ci<=? #\b #\A)  #f]
            [assert (char-ci<=? #\9 #\a)  #t]
            [assert (char-ci<=? #\9 #\A)  #t]

            [assert (char>=? #\b #\a)  #t]
            [assert (char>=? #\a #\a)  #t]
            [assert (char>=? #\a #\b)  #f]
            [assert (char>=? #\a #\9)  #t]
            [assert (char>=? #\A #\9)  #t]

            [assert (char-ci>=? #\b #\a)  #t]
            [assert (char-ci>=? #\a #\a)  #t]
            [assert (char-ci>=? #\A #\a)  #t]
            [assert (char-ci>=? #\a #\b)  #f]
            [assert (char-ci>=? #\a #\9)  #t]
            [assert (char-ci>=? #\A #\9)  #t]

            [assert (char-alphabetic? #\9)   #f]
            [assert (char-alphabetic? #\a)   #t]
            ;; [assert (char-alphabetic? #\漢)  #f]

            [assert (char-whitespace? #\ )  #t]
            ;; [assert (char-whitespace? #\\t)  #t]

;;     ARG1(SCH_CHAR('\n'));
;;     CU_ASSERT( subr_char_is_whitespace(1,1) == SCH_TRUE );

;;     ARG1(SCH_CHAR('\f'));
;;     CU_ASSERT( subr_char_is_whitespace(1,1) == SCH_TRUE );

;;     ARG1(SCH_CHAR('\r'));
;;     CU_ASSERT( subr_char_is_whitespace(1,1) == SCH_TRUE );

;;     ARG1(SCH_CHAR('a'));
;;     CU_ASSERT( subr_char_is_whitespace(1,1) == SCH_FALSE );

;; /*     ARG1(SCH_CHAR('構')); */
;; /*     CU_ASSERT( subr_char_is_whitespace(1,1) == SCH_FALSE ); */

            [assert (char-lower-case? #\a)  #t]
            [assert (char-lower-case? #\A)  #f]
            [assert (char-lower-case? #\:)  #f]

            [assert (char-upper-case? #\a)  #f]
            [assert (char-upper-case? #\A)  #t]
            [assert (char-upper-case? #\:)  #f]

            [assert (char-numeric? #\a)  #f]
            [assert (char-numeric? #\0)  #t]
            [assert (char-numeric? #\9)  #t]
            ;; [assert (char-numeric? #\\t)  #f]

            [assert (char-upcase #\a)  #\A]
            [assert (char-upcase #\B)  #\B]
            [assert (char-upcase #\:)  #\:]

            [assert (char-downcase #\A)  #\a]
            [assert (char-downcase #\z)  #\z]
            [assert (char-downcase #\%)  #\%]

))