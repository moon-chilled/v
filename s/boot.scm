;(set! (*s7* 'safety) 2)
;(format #t "~a~%~a~%" (signature map) (signature LOW-create-binding))
;(display (signature map))
;(display (signature LOW-create-binding))

(load "prelude.scm")

(define *motions* (inlet))

(defmacro with-cursor-location (ps :rest body)
              (let ((yx (gensym)))
                `(let* ((,yx (cursor-location))
                        (,(car ps) (car ,yx))
                        (,(cadr ps) (cdr ,yx)))
                   ,@body)))

(defmacro define-motion (name :rest body)
              `(with-let *motions*
                         (define (,name) (with-cursor-location (y x) ,@body))))
(defmacro bind-motion (motion key)
              `(LOW-create-binding 'motion ,key (LOW-make-motion (*motions* ',motion))))

(defun isspace (c) (if (char-position c " \t\n\r\f") #t #f))

(define-motion cleft (cons y (if (> x 0) (1- x) x)))
(define-motion cdown
               (let ((y (if (>= (1+ y) (line-count)) y (1+ y))))
                 (cons y (min x (character-count y)))))
(define-motion cup
               (let ((y (if (> y 0) (1- y) y)))
                 (cons y (min x (character-count y)))))
(define-motion cright (cons y (if (>= x (character-count y)) x (1+ x))))
(define-motion eol (cons y (character-count y)))
(define-motion bol (cons y 0))
(define-motion bof '(0 . 0))
(define-motion eof (let ((l (1- (line-count))))
                         (cons l (character-count l))))
(define-motion word-forward
               (loop while (and (< x (character-count y)) (not (isspace (character-at y x))))
                     do (inc! x))
               (loop while (and (< x (character-count y) )(isspace (character-at y x)))
                     do (inc! x))
               (cons y x))
(define-motion word-back
               (loop while (and (> x 0) (isspace (character-at y (1- x))))
                     do (dec! x))
               (loop while (and (> x 0) (not (isspace (character-at y (1- x)))))
                     do (dec! x))
               (cons y x))

(bind-motion cleft #\h)
(bind-motion cdown #\j)
(bind-motion cup #\k)
(bind-motion cright #\l)
(bind-motion eol #\$)
(bind-motion bol #\0)
(bind-motion bof #\g)
(bind-motion eof #\G)
(bind-motion word-forward #\w)
(bind-motion word-back #\b)
