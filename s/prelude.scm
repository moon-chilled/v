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

(defexpansion add-reader (character :rest body)
              (let ((str (gensym)))
                `(set! *#readers*
                   (cons (cons ,character (lambda (,str)
                                            (decf (port-position (current-input-port)) (1- (length ,str)))
                                            ,@body)) *#readers*))))

(defexpansion prog1 (form1 :rest forms)
              (let ((sym (gensym)))
                `(let ((,sym ,form1))
                   ,@forms
                   ,sym)))

; todo unicode character syntax (→ integer); maybe #_á?  Or overload #\á?  #'á
; looks like cl but is legitimately really cute

; unescaped strings, with syntax like
; #q/foo/
; #q|bar\|
; #q{these {do} nest}
(add-reader #\q
            (let* ((openers "[{(<")
                   (closers "]})>")
                   (open-delimiter (read-char))
                   (close-delimiter (let ((i (char-position open-delimiter openers)))
                                      (if i (string-ref closers i) open-delimiter)))
                   (depth 1))
              (prog1
                (apply string (loop for c = (peek-char) then (peek-char)
                                    do (apply case c
                                              `(((#<eof>) (error 'string-read-error "unexpected end of file in delimited string"))
                                                ((,close-delimiter) (decf depth))
                                                ((,open-delimiter) (incf depth))))
                                    while (> depth 0)
                                    collect (read-char)))
                ; read the close-delimiter
                (read-char))))
(add-reader #\L `(lambda ($) ,(read)))

(format #t "'~a' '~a' '~a' '~a'~%" #q/ab\c/#q/r/ #q{ab cd {de }f } #q//)
