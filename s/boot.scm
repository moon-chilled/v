;(set! (*s7* 'safety) 2)

(load "prelude.scm")

(define *motions* (inlet))
(define *mutations* (inlet))

(defmacro define-motion (name :rest body)
  `(with-let *motions*
             (define (,name) ,@body)))
(defmacro bind-motion (motion key)
              `(LOW-create-binding 'motion ,key (LOW-make-motion (*motions* ',motion))))

(defun isspace (c) (if (char-position c " \t\n\r\f") #t #f))

(define-motion cleft
               (let ((it (iterate 'stop-after-newline #f #f)))
                 (iterator-read it #t)
                 (iterator-loc it)))
(define-motion cright
               (let ((it (iterate 'stop-before-newline #t #f)))
                 (unless (iterator-out it) (iterator-read it #t))
                 (iterator-loc it)))

(define-motion cdown
               (let ((y (if (>= (1+ y) (line-count)) y (1+ y))))
                 (cons y (min x (character-count y)))))
(define-motion cup
               (let ((y (if (> y 0) (1- y) y)))
                 (cons y (min x (character-count y)))))
(define-motion eol (cons y (character-count y)))
(define-motion bol (cons y 0))
(define-motion bof '(0 . 0))
(define-motion eof (let ((l (1- (line-count))))
                         (cons l (character-count l))))
(define-motion word-forward
               (loop while (and (< x (character-count y)) (not (isspace (character-at y x))))
                     do (incf x))
               (loop while (and (< x (character-count y) )(isspace (character-at y x)))
                     do (incf x))
               (cons y x))
(define-motion word-back
               (loop while (and (> x 0) (isspace (character-at y (1- x))))
                     do (decf x))
               (loop while (and (> x 0) (not (isspace (character-at y (1- x)))))
                     do (decf x))
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


(defmacro define-mutation (name bindings perform undo)
  `(let ,(map (lambda (x) (list (car x) #<undefined>)) bindings) #L`(,(car $) #<undefined>)
     (with-let *mutations*
               (define ,name (list
                               ; prepare
                               (lambda () ,(map (lambda (x) `(set! ,@x)) bindings)) ; ,(map #L`(set! ,@$))
                               ,perform
                               ,undo)))))
(defmacro bind-mutation (mutation key)
              `(LOW-create-binding 'mutation ,key (apply LOW-make-mutation (*mutations* ',mutation))))

(define-mutation delforward
                 ()
                 (lambda () (LOW-text-remove (car (cursor-location)) (cdr (cursor-location)) 1))
                 (lambda () #f)) ;todo
(bind-mutation delforward #\x)
;
;
;
;(lambda ()
;  (let ((x (cdr (cursor-location))))
;    (values
;      (lambda () (... x))
;      (lambda () (... x)))))
;
;(define-mutation
;  ((x (cdr cursor-location))
;   (... ...))
;  (lambda () (... x))
;  (lambda () (... x)))
;
;(let ((x #<undefined>))
;  (values
;    (lambda () (set! x (cdr cursor-location)))
;    (lambda () (... x))
;    (lambda () (... x))))
