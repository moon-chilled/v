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

(defun c-parse (it old-state)
  (let ((c (iterator-read it #f)))
    (cond
      ((string=? "/" c)
       (iterator-read it #t)
       (let ((c (iterator-read it #f)))
         (cond
           ((string=? c "/")
            (loop until (or (iterator-out it)
                            (string=? "\n" (iterator-read it #f)))
                  do (iterator-read it #t))
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
       (iterator-read it #t)
       (loop until (or (iterator-out it)
                       (string=? "\"" (iterator-read it #t))))
       'string)
      ((char-position c "+=!%^&*-|~<>?:-") (iterator-read it #t) 'operator)
      ((char-position c "()[]{};") (iterator-read it #t) 'operator) ;not really...
      (#t
       (iterator-read it #t)
       (let ((sep "/* \t\n+=!%^&-|~<>?:-[]{}();")) ;\f\v\r
         (loop until (or (iterator-out it)
                         (char-position (iterator-read it #f) sep))
               do (iterator-read it #t))
         'identifier)))))

(defun c-parse (it old-state)
  (let ((c (iterator-read it #f)))
    (cond
      ((string=? "/" c)
       (iterator-read it #t)
       (let ((c (iterator-read it #f)))
         (cond
           ((string=? c "/")
            (let loop ()
              (unless (or (iterator-out it)
                          (string=? "\n" (iterator-read it #f)))
                (iterator-read it #t)
                (loop)))
            'comment)
           ((string=? c "*")
            (iterator-read it #t)
            (let loop ()
              (unless (or (iterator-out it)
                          (and (string=? "*" (iterator-read it #t))
                               (not (iterator-out it))
                               (string=? "/" (iterator-read it #t))))
                (loop)))
            'comment)
           (#t 'operator))))
      ((isspace c)
       (let loop ()
         (unless (or (iterator-out it)
                     (not (isspace (iterator-read it #f))))
           (iterator-read it #t)
           (loop)))
       'neutral)
      ((string=? c "\"")
       (iterator-read it #t)
       (let loop ()
         (unless (or (iterator-out it)
                     (string=? "\"" (iterator-read it #t)))
           (loop)))
       'string)
      ((char-position c "+=!%^&*-|~<>?:-") (iterator-read it #t) 'operator)
      ((char-position c "()[]{};") (iterator-read it #t) 'operator) ;not really...
      (#t
       (let ((id (iterator-read it #t)))
         (let ((sep "/* \t\n+=!%^&-|~<>?:-[]{}();")) ;\f\v\r
           (let loop ()
             (unless (or (iterator-out it)
                         (char-position (iterator-read it #f) sep))
               (set! id (append id (iterator-read it #t)))
               (loop)))
           (let ((keywords (map symbol->string '(for do while if else break continue default goto)))
                 (builtin-types (map symbol->string '(void char short int long signed unsigned float double  struct union))))
             (cond
               ((member id keywords) 'keyword)
               ((member id builtin-types) 'builtin-type)
               (#t 'identifier)))))))))

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
