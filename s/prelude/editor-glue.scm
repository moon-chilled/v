;(set! (*s7* 'safety) 2)

(define *motions* (inlet))
(define *mutations* (inlet))
(define *higher-order-functions* (inlet))

(defmacro define-motion (name :rest body)
  `(with-let *motions*
             (define (,name) ,@body)))
(defmacro bind-motion (motion key)
              `(LOW-create-binding 'motion ,key (LOW-make-motion (*motions* ',motion))))


(defmacro define-mutation (name bindings perform undo)
  `(with-let *mutations*
             (define ,name
               (let ,(map #L`(,(car $) #<undefined>) bindings)
                 (list
                   (lambda () ,@(map #L`(set! ,@$) bindings)) ;prepare
                   ,perform
                   ,undo)))))
(defmacro bind-mutation (mutation key)
              `(LOW-create-binding 'mutation ,key (apply LOW-make-mutation (*mutations* ',mutation))))

(defun expand-hof (signature fn args modes ret)
  (if (null? signature)
     (cond
       ((eq? ret 'motion)
        `(LOW-make-motion (lambda () (,fn ,@args))))
       ((eq? ret 'mutation)
        `(LOW-make-mutation (lambda () ()) (lambda () (,fn ,@args)) (lambda () (error))))
       (#t (error 'simple-error "bad type ~a" ret)))
     (let* ((narg (cond ((eq? (car signature) 'str) 1)
                        ((eq? (car signature) 'motion) 1)
                        (#t (error 'simple-error "~a not a valid type" (car signature)))))
            (targs (times narg (gensym))))
       `(LOW-make-higher-order-function 
          ',(car modes)
          ',(cons ret signature)
          (lambda ,targs ,(expand-hof (cdr signature) fn (append args targs) (cdr modes) ret))))))

(defmacro define-higher-order-function (name ret modes signature params :rest body)
  `(with-let *higher-order-functions*
             (define ,name
               (list ',ret ,(expand-hof signature `(lambda ,params ,@body) () modes ret)))))
(defmacro bind-higher-order-function (hof key)
  `(LOW-create-binding ',(let ((fret (car (*higher-order-functions* hof))))
                           (cond
                             ((symbol? fret) fret)
                             ((pair? fret) 'function)
                             (#t (error 'simple-error "~a not a valid return type in signature" fret))))
                       ,key
                       (cadr (*higher-order-functions* ',hof))))

(defmacro bind-insertion (category name key)
  `(LOW-create-binding 'insert ,key ,(cond
                                       ((eq? category 'motion) `(LOW-make-motion (*motions* ',name)))
                                       ((eq? category 'mutation) `(apply LOW-make-mutation (*mutations* ',name)))
                                       (#t (error 'wrong-type-arg "~a must be either motion or mutation" category)))))

(define *v-init-hooks* ())
(defun do-init-v () (loop for f in *v-init-hooks* do (f)))
(defun add-v-init-hook (f) (push f *v-init-hooks*))
