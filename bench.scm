(define prof-avr
  (lambda(prefix time)
    (define realtime?
      (lambda(line)
        (rxmatch #/real/ line)))
    (define gettime
      (lambda(line)
        (let ([time (rxmatch-after (rxmatch #/real\t/ line))])
          (let ([min (string->number ((rxmatch #/.*real[ \t]*([0-9]+)m([0-9.]+)s/ line) 1))]
                [sec (string->number ((rxmatch #/.*real[ \t]*([0-9]+)m([0-9.]+)s/ line) 2))])
            (+ (* min 60) sec)))))
    (define realseconds
      (lambda(fname)
        (call-with-input-file fname
          (lambda(p)
            (let loop ([line (read-line p)])
              (if (eof-object? line)
                  'FAILED
                  (begin
                    (if (realtime? line)
                        (gettime line)
                        (loop (read-line p))))))))))
    (let loop ([i     0]
               [total 0])
      (if (< i time)
          (loop (+ 1 i)
                (+ total (realseconds (string-append prefix (number->string i)))))
          (/ total time)))))

(define main
  (lambda(args)
    (format #t "~A times average~%" (cadr args))
    (format #t "FIB : ~A~%" (prof-avr "./prof/prof_fib_" 10))
    (format #t "TAK : ~A~%" (prof-avr "./prof/prof_tak_" 10))
    (format #t "P1  : ~A~%" (prof-avr "./prof/prof_p1_"  10))
    ))
