; To be completely honest, I didn't entirely understand the yi paper.
; The actual yi codebase seems to use a lexer generator, which is not helpeful.
; _SO_, rather than spending altogether far too much time and energy on junk
; like incremental parsers, I'm just gonna hack something up that works well
; enough.

; Let's say:
; 
; State1 :: arbitrary --for example, syntax highlighting
; State2 :: arbitrary --for example, indentation
; State3 :: arbitrary --for example, semantic analysis

; parse ::  input -> last state -> (some tail of input,[spans of state1],[spans of state2])
; parse' :: input -> last state -> [spans of state1] -> (some tail of input,[spans of state3])
; for now, stick with:
; parse :: iterator -> last state -> state --state applies to everything read from the iterator

; for now, just ignore the old state; we're tokenizing, not parsing
; c states:
; neutral identifier keyword builtin-type line-comment block-comment string

(define +c-keywords+ (apply hash-table (map (lambda (x) (values (symbol->string x) #t)) '(for do while if else break continue default goto return switch))))
(define +c-builtin-types+ (apply hash-table (map (lambda (x) (values (symbol->string x) #t))
                                                 '(void _Bool bool char short int long signed unsigned float double
                                                   int8_t uint8_t int16_t uint16_t int32_t uint32_t int64_t uint64_t
                                                   __int128_t __uint128_t
                                                   struct union
                                                   typedef extern static _Thread_local thread_local auto register
                                                   const volatile restrict _Atomic
                                                   inline _Noreturn noreturn))))

(defun c-parse (it old-state)
  (let ((c (iterator-read it #t)))
    (cond
      ((string=? "/" c)
       (let ((c (iterator-read it #f)))
         (cond
           ((string=? c "/")
            (loop until (or (iterator-out it)
                            (string=? "\n" (iterator-read it #f)))
                  do (iterator-read it #t)) ;\n is not part of the comment
            'comment)
           ((string=? c "*")
            (iterator-read it #t)
            (loop until (or (iterator-out it)
                            (and (string=? "*" (iterator-read it #t))
                                 (not (iterator-out it))
                                 (string=? "/" (iterator-read it #t)))))
            'comment)
           (#t 'operator))))
      ((isspace c)
       (loop until (or (iterator-out it)
                       (not (isspace (iterator-read it #f))))
             do (iterator-read it #t))
       'neutral)
      ((string=? c "\"")
       (loop until (or (iterator-out it)
                       (string=? "\"" (iterator-read it #t))))
       'string)
      ((char-position c "+=!%^&*-|~<>?:-") 'operator)
      ((char-position c "()[]{};") 'operator) ;not really...
      (#t
       (let ((id c))
         (let ((sep "/* \t\n+=!%^&-|~<>?:-[]{}();")) ;\f\v\r
           (loop until (or (iterator-out it)
                           (char-position (iterator-read it #f) sep))
                 do (set! id (append id (iterator-read it #t))))
           (cond
             ((+c-keywords+ id) 'keyword)
             ((+c-builtin-types+ id) 'builtin-type)
             (#t 'identifier))))))))

(defun state-to-colour (state)
  (case state
    ((neutral operator) #xffffff)
    ((identifier) #xffffff)
    ((comment) #x4c4cff)
    ((string) #xff0000)
    ((keyword) #xffff00)
    ((builtin-type) #x00ff00)
    (else #x00ff00)))

(add-v-init-hook (lambda () (set-highlighter 'neutral c-parse state-to-colour)))
