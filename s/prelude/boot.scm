(define loop (let () (load "prelude/loop.scm" (curlet)) loop))
(load "prelude/language.scm")
(load "prelude/editor-glue.scm")

(load "test/test.scm")

; vvv these are not 'boot' and should be loaded from somewhere better
(load "text/text.scm")
(load "vi/vi.scm")
(load "syntax/syntax.scm")
