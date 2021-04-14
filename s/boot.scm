;(set! (*s7* 'safety) 2)

;(set-current-error-port *stdout*)
;(set-current-output-port (open-output-file "s7o.log"))

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
               (let* ((c (cursor-location))
                      (y (1+ (c 'y))))
                 (if (>= y (line-count))
                   c
                   (cursor-at y (min (c 'gx) (grapheme-count y))))))
(define-motion cup
               (let* ((c (cursor-location))
                      (y (1- (c 'y))))
                 (if (< y 0)
                   c
                   (cursor-at y (min (c 'gx) (grapheme-count y))))))
(define-motion eol (let ((y ((cursor-location) 'y)))
                     (UNSAFE-create-cursor y (grapheme-count y) (byte-count y))))
(define-motion bol (UNSAFE-create-cursor ((cursor-location) 'y) 0 0))
(define-motion bof (UNSAFE-create-cursor 0 0 0))
(define-motion eof (let ((y (1- (line-count))))
                     (UNSAFE-create-cursor y (grapheme-count y) (byte-count y))))
; vvv this can be prettier with some nice macros
(define-motion word-forward
               (let ((it (iterate 'stop-before-newline #t #f)))
                 (loop until (or (iterator-out it) (isspace (iterator-read it #t))))
                 (loop until (or (iterator-out it) (not (isspace (iterator-read it #f))))
                       do (iterator-read it #t))
                 (iterator-loc it)))
(define-motion word-back
               (let* ((it (iterate 'stop-after-newline #f #f)))
                 (loop until (or (iterator-out it) (not (isspace (iterator-read it #t)))))
                 (loop until (or (iterator-out it) (isspace (iterator-read it #f)))
                       do (iterator-read it #t))
                 (iterator-loc it)))

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
  `(with-let *mutations*
             (define ,name
               (let ,(map (lambda (x) (list (car x) #<undefined>)) bindings) ;#L`(,(car $) #<undefined>)
                 (list
                   (lambda () ,@(map #L`(set! ,@$) bindings)) ;prepare
                   ,perform
                   ,undo)))))
(defmacro bind-mutation (mutation key)
              `(LOW-create-binding 'mutation ,key (apply LOW-make-mutation (*mutations* ',mutation))))
(define-mutation delbackward
                 ((ch-loc (let ((it (iterate 'stop-after-newline #f #f)))
                            (cons (if (not (iterator-out it)) (iterator-read it #t) "")
                                  (iterator-loc it)))))
                 (lambda () (LOW-text-remove (cdr ch-loc)))
                 (lambda () (LOW-text-insert (cdr ch-loc) (car ch-loc))))
(define-mutation delforward
                 ((ch-loc (let ((it (iterate 'stop-before-newline #t #f)))
                            (cons (if (not (iterator-out it)) (iterator-read it #t) "")
                                  (iterator-loc it)))))
                 (lambda () (LOW-text-remove (cdr ch-loc)))
                 (lambda () (LOW-text-insert (cdr ch-loc) (car ch-loc))))
(bind-mutation delforward #\x)
(bind-mutation delbackward #\X)
