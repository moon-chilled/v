; NB. all can be much faster if x is large or f has known arity

(defun bindr (f :rest x)
  (lambda y
    (apply f (append y x))))

(defun bindl (f :rest x)
  (lambda y
    (apply f (append x y))))

(defun funcall (f :rest xs) (apply f xs))

(define every
  (letrec ((every* (lambda (f xs)
                     (if (null? xs)
                       #t
                       (if (not (f (car xs)))
                         #f
                         (every* (cdr xs)))))))
    (lambda (f :rest xs)

      (let lp ((xs xs))
        (if (null? (caar xs))
          (or (every* null? xs) (error "'every' arguments not of equal length"))
          (if (not (apply f (map car xs)))
            #f
            (lp (map cdr xs))))))))

(defun some (f :rest xs)
  (let lp ((xs xs))
    (if (null? (caar xs))
      (and (or (every* null? xs) (error "'some arguments not of equal length"))
           #f)
      (if (apply f (map car xs))
        #t
        (lp (map cdr xs))))))

(defun foldl (f xs)
  (let lp ((acc (car xs))
           (xs (cdr xs)))
    (if (null? xs)
      acc
      (lp (f acc (car xs))
          (cdr xs)))))

(defun foldr (f xs)
  (if (null? (cdr xs))
    (car xs)
    (f (car xs) (foldr f (cdr xs)))))

(defun composel (f :rest g)
  (let ((last (last g)))
    (set! last (set! (cdr last) (cons () ())))
    (lambda x
      (apply f (map
                 (lambda (x) (set! (car last) x) (foldr funcall g))
                 x)))))

(defun last (x)
  (if (null? (cdr x))
    x
    (last (cdr x))))

(defun composer (f :rest g)
  (if (null? g)
    f
    (let* ((f (cons f g))
           (last (last f))
           (g (car last)))
      (lambda x
        (set! (car last) (apply g x))
        (foldr funcall f)))))
