; basic language augmentations
; (sorry guy steele)

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
; #'x ←→ (char->integer #\x), but with utf8 decoding
(add-reader #\'
            (letrec ((read-continuation-bytes (macro (res n)
                                                     (let ((s (gensym)))
                                                       `(let ((,s (read-byte)))
                                                          (unless (<= ,s #b10111111) (error 'utf8-error "bad continuation byte"))
                                                          (set! ,res (+ (ash ,res 6) (logand ,s #b01111111)))
                                                          ,(if (> n 1)
                                                             `(read-continuation-bytes ,res ,(1- n))
                                                             res))))))
              (let ((b0 (read-byte)))
                (cond
                  ((<= b0 #b01111111) b0)
                  ((<= b0 #b11011111) (set! b0 (logand b0 #b00111111))
                                      (read-continuation-bytes b0 1))
                  ((<= b0 #b11101111) (set! b0 (logand b0 #b00011111))
                                      (read-continuation-bytes b0 2))
                  ((<= b0 #b11110111) (set! b0 (logand b0 #b00001111))
                                      (read-continuation-bytes b0 3))
                  (#t (error 'utf8-error "bad initial byte"))))))

(defun utf8-encode (c)
  (cond
    ((< c 128) (string (integer->char c)))
    ((< c 2048) (let ((b1 (logand c #b111111))
                      (b0 (ash c -6)))
                  (format #t "~a / ~a / ~a~%" c b0 b1)
                  (string (integer->char (logior b0 #b11000000))
                          (integer->char (logior b1 #b10000000)))))
    ((< c 65536) (let ((b2 (logand c #b111111))
                       (b1 (logand (ash c -6) #b111111))
                       (b0 (ash c -12)))
                   (string (integer->char (logior b0 #b11100000))
                           (integer->char (logior b1 #b10000000))
                           (integer->char (logior b2 #b10000000)))))
    ((< c 2097152) (let ((b3 (logand c #b111111))
                         (b2 (logand (ash c -6) #b111111))
                         (b1 (logand (ash c -12) #b111111))
                         (b0 (ash c -18)))
                     (string (integer->char (logior b0 #b11110000))
                             (integer->char (logior b1 #b10000000))
                             (integer->char (logior b2 #b10000000))
                             (integer->char (logior b3 #b10000000)))))
    (#t (error 'utf8-error "codepoint out of range (must be <2²¹)"))))

;(load "test.scm")