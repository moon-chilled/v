;(set! (*s7* 'safety) 2)

(define *motions* (inlet))
(define *mutations* (inlet))

(defmacro define-motion (name :rest body)
  `(with-let *motions*
             (define (,name) ,@body)))
(defmacro bind-motion (motion key)
              `(LOW-create-binding 'motion ,key (LOW-make-motion (*motions* ',motion))))


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

(defmacro bind-insertion (category name key)
  `(LOW-create-binding 'insert ,key ,(cond
                                       ((eq? category 'motion) `(LOW-make-motion (*motions* ',name)))
                                       ((eq? category 'mutation) `(apply LOW-make-mutation (*mutations* ',name)))
                                       (#t (error 'wrong-type-arg "~a must be either motion or mutation" category)))))
