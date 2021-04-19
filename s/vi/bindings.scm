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

(bind-mutation delforward #\x)
(bind-mutation delbackward #\X)

(bind-insertion motion cleft 'left)
(bind-insertion motion cright 'right)
(bind-insertion motion cup 'up)
(bind-insertion motion cdown 'down)

(bind-insertion mutation delforward 'delete)
(bind-insertion mutation delbackward 'backspace)


(LOW-create-binding 'function #\t
                    (LOW-make-higher-order-function
                      'insert
                      '(motion str)
                      (lambda (text)
                        (LOW-make-motion (lambda ()
                                           (let ((it (iterate 'stop-before-newline #t #f)))
                                             (loop until (or (iterator-out it) (string=? text (iterator-read it #f)))
                                                   do (iterator-read it #t))
                                             (iterator-loc it)))))))

