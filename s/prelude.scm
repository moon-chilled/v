(define loop (let () (load "loop.scm" (curlet)) loop))

; cl, my maven!
; perhaps I'll ?-p and !f too, but later
(define-expansion (defexpansion name pspec . body) `(define-expansion* (,name ,@pspec) ,@body))
(defexpansion lambda (pspec :rest body) (if (string? (car body))
                                          `(let ((+documentation+ ,(car body))) (lambda* ,pspec ,@(cdr body)))
                                          `(lambda* ,pspec ,@body)))
(defexpansion macro (pspec :rest body) (if (string? (car body))
                                          `(let ((+documentation+ ,(car body))) (macro* ,pspec ,@(cdr body)))
                                          `(macro* ,pspec ,@body)))
(defexpansion defun (name pspec :rest body) (if (string? (car body))
                                              `(define ,name (let ((+documentation+ ,(car body))) (lambda (,@pspec) ,@(cdr body))))
                                              `(define* (,name ,@pspec) ,@body)))
(defexpansion defmacro (name pspec :rest body) (if (string? (car body))
                                                 `(define ,name (let ((+documentation+ ,(car body))) (macro (,@pspec) ,@(cdr body))))
                                                 `(define-macro* (,name ,@pspec) ,@body)))
(defun 1+ (x) (+ x 1))
(defun 1- (x) (- x 1))
(defexpansion incf (x (inc 1))
              `(set! ,x (+ ,x ,inc)))
(defexpansion decf (x (dec 1))
              `(set! ,x (- ,x ,dec)))

(defexpansion block (:rest body)
              `(call-with-exit (lambda (return) ,@body)))
(defexpansion assert (c) `(unless ,c (error 'assertion-failure (format #f "unhandled assertion ~a" ',c))))

(defexpansion add-reader (character str :rest body)
              `(set! *#readers*
                (cons (cons ,character (lambda (,str) ,@body)) *#readers*)))

; todo unicode character syntax (→ integer); maybe #_á?  Or overload #\á?  #'á
; looks like cl but is legitimately really cute

; unescaped strings, with syntax like
; #q/foo/
; #q|bar\|
; #q{these {do} nest}
; note: at the moment this is wrong; something like #/foo/bar will be
; truncated to "foo" when it should be (values "foo" bar).  The proper fix
; involves creating a wrapper port, or (my preference) amending behaviour of
; *#readers* to be sensible when presented with a niladic function
(add-reader #\q str
            (let* ((openers "[{(<")
                   (closers "]})>")
                   (open-delimiter (string-ref str 1))
                   (close-delimiter (let ((i (char-position open-delimiter openers)))
                                      (if i (string-ref closers i) open-delimiter)))
                   (r "")
                   (s (substring str 2))
                   (depth 1))
              (block
                (let ((f (lambda (c)
                           (cond
                             ((char=? c close-delimiter) (decf depth))
                             ((char=? c open-delimiter) (incf depth)))
                           (if (> depth 0)
                             (set! r (string-append r (string c)))
                             (return r)))))
                  (loop for i from 0 below (length s)
                        do (f (string-ref s i)))
                  (loop for c = (read-byte) then (read-byte)
                        do (begin (when (eq? c #<eof>) (error 'string-read-error "unexpected end of file in delimited string"))
                                  (f (integer->char c))))))))

(format #t "This still works: '~a'~%" #q|foo bar {baz } biz|)
(format #t "This still works: '~a'~%" #q{foo bar {baz } biz})
;(format #t "This doesn't work yet: '~a' '~a'~%" #q/foo/#q/bar/)
