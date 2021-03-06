(define-motion cleft
               (let ((it (iterate 'stop-after-newline #f #f)))
                 (unless (iterator-out it) (iterator-read it #t))
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
               (let ((it (iterate 'eat-everything #t #f)))
                 (loop until (or (iterator-out it) (isspace (iterator-read it #t))))
                 (loop until (or (iterator-out it) (not (isspace (iterator-read it #f))))
                       do (iterator-read it #t))
                 (iterator-loc it)))
(define-motion word-back
               (let* ((it (iterate 'eat-everything #f #f)))
                 (loop until (or (iterator-out it) (not (isspace (iterator-read it #t)))))
                 (loop until (or (iterator-out it) (isspace (iterator-read it #f)))
                       do (iterator-read it #t))
                 (iterator-loc it)))
(define-motion paragraph-forward
               (let ((it (iterate 'eat-everything #t #f)))
                 (loop until (or (iterator-out it) (not (string=? "\n" (iterator-read it #t)))))
                 (loop until (or (iterator-out it) (and (string=? "\n" (iterator-read it #t))
                                                        (or (iterator-out it) (string=? "\n" (iterator-read it #f))))))
                 (iterator-loc it)))
(define-motion paragraph-back
               (let ((it (iterate 'eat-everything #f #f)))
                 (loop until (or (iterator-out it) (not (string=? "\n" (iterator-read it #t)))))
                 (loop until (or (iterator-out it) (and (string=? "\n" (iterator-read it #t))
                                                        (or (iterator-out it) (string=? "\n" (iterator-read it #f))))))
                 (iterator-loc it)))


(define-mutation delbackward
                 ((ch-loc (let ((it (iterate 'stop-after-newline #f #f)))
                            (cons (if (not (iterator-out it)) (iterator-read it #t) "")
                                  (iterator-loc it)))))
                 (lambda () (LOW-text-remove (cdr ch-loc)))
                 (lambda () (LOW-text-insert (cursor-location) (car ch-loc))))
(define-mutation delforward
                 ((ch-loc (let ((it (iterate 'stop-before-newline #t #f)))
                            (cons (if (not (iterator-out it)) (iterator-read it #t) "")
                                  (iterator-loc it)))))
                 (lambda () (LOW-text-remove (cdr ch-loc)))
                 (lambda () (LOW-text-insert (cursor-location) (car ch-loc))))

(define-mutation insert-mode
                 ((m (current-mode)))
                 (lambda () (LOW-change-mode 'insert))
                 (lambda () (LOW-change-mode m)))
(define-mutation normal-mode
                 ((m (current-mode)))
                 (lambda () (LOW-change-mode 'normal))
                 (lambda () (LOW-change-mode m)))

(define-higher-order-function til motion
                              (insert)
                              (str)
                              (text)
                              (or (loop with it = (iterate 'stop-before-newline #t #f)
                                        until (iterator-out it)
                                        thereis (and (string=? text (iterator-read it #f))
                                                     (iterator-loc it))
                                        do (iterator-read it #t))
                                  (cursor-location)))
(define-higher-order-function find motion
                              (insert)
                              (str)
                              (text)
                              (or (loop with it = (iterate 'stop-before-newline #t #f)
                                        until (iterator-out it)
                                        thereis (and (string=? text (iterator-read it #t))
                                                     (iterator-loc it)))
                                  (cursor-location)))

(define-higher-order-function til-back motion
                              (insert)
                              (str)
                              (text)
                              (or (loop with it = (iterate 'stop-after-newline #f #f)
                                        until (iterator-out it)
                                        thereis (and (string=? text (iterator-read it #f))
                                                     (iterator-loc it))
                                        do (iterator-read it #t))
                                  (cursor-location)))

(define-higher-order-function find-back motion
                              (insert)
                              (str)
                              (text)
                              (or (loop with it = (iterate 'stop-after-newline #f #f)
                                        until (iterator-out it)
                                        thereis (and (string=? text (iterator-read it #t))
                                                     (iterator-loc it)))
                                  (cursor-location)))

(define-higher-order-function delete mutation
                              (motion)
                              (motion)
                              (loc)
                              (LOW-text-remove (loc)))

(define-higher-order-function change mutation
                              (motion)
                              (motion)
                              (loc)
                              (LOW-text-remove (loc))
                              (LOW-change-mode 'insert))
