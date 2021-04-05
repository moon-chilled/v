;(set! (*s7* 'safety) 2)
;(format #t "~a~%~a~%" (signature map) (signature LOW-create-binding))
;(display (signature map))
;(display (signature LOW-create-binding))
(define-expansion (define-motion char . body)
                  `(LOW-create-binding 'motion ,char (LOW-make-motion (lambda () ,@body))))

(define-motion #\$
               (let* ((yx (cursor-location))
                      (y (car yx))
                      (x (cdr yx)))
                 (cons y (character-count y))))
(define-motion #\0
               (let ((y (car (cursor-location))))
                 (cons y 0)))
(define-motion #\g
               '(0 . 0))
(define-motion #\G
               (let ((l (- (line-count) 1)))
                 (cons l (character-count l))))
