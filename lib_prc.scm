(define call-with-input-file
  (lambda(filename proc)
    (let* ((p   (open-input-file filename))
           (ret (proc p)))
      (close-input-port p)
      ret)))

(define call-with-output-file
  (lambda(filename proc)
    (let* ((p   (open-output-file filename))
           (ret (proc p)))
      (close-output-port p)
      ret)))

(define with-input-from-file
  (lambda(filename proc)
    (let* ((port  (open-input-file filename))
           (stdin (current-input-port port))
           (ret   (proc)))
      (close-input-port port)
      (current-input-port stdin)
      ret)))

(define with-output-to-file
  (lambda(filename proc)
    (let* ((port   (open-output-file filename))
           (stdout (current-output-port port))
           (ret    (proc)))
      (close-output-port port)
      (current-output-port stdout)
      ret)))

