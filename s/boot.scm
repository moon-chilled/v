(define-expansion (define-motion char . body)
                  `(LOW-create-binding ,char (LOW-make-motion (lambda () ,@body))))

(define-motion #\g
               '(0 . 0))
