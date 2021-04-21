(defmacro assert (x)
  `(unless ,x
     (error 'assertion-failure ,(format #f "assertion ~a failed" x))))

;(format #t "have ~a~%" (macroexpand (assert #t)))
(let ((unitab '((#'a 97 "a")
                (#'á 225 "á")
                (#'⍝ 9053 "⍝")
                (#'𐀏 65551 "𐀏"))))
  (map (lambda ($)
         (assert (= (car $) (cadr $)))
         (assert (string=? (utf8-encode (car $)) (caddr $))))
       unitab))
(assert (equal? '(#q/ab\c/#q/r\/ #q{ab cd {de }f } #q//)
                '(  "ab\\c" "r\\"  "ab cd {de }f "   "")))
