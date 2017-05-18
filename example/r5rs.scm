;;; R5RS.scm -- a Scheme tokenizer under R5RS definition
;;;
;;; 
;;; Reference:
;;;   + http://www.schemers.org/Documents/Standards/R5RS/r5rs.pdf
;;;   + http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-10.html#%_chap_7

(load-relative "../slex.scm")

(define-lex r5rs-L
  (definition*
    
    ; digit               -> [0-9]
    ; letter              -> [a-zA-Z]
    ; special-initial     -> ! | $ | % | & | * | / | : | < | = | > | ? | ^ | _ | ~
    ; special-subsequent  -> + | - | . | @
    ; peculiar-identifier -> + | - | ...
    (digit               (sig slex:digit))
    (letter              (sig slex:alpha))
    (special-initial     (sig* #\! #\$ #\% #\& #\* #\/ #\: 
                               #\< #\= #\> #\? #\^ #\_ #\~))
    (special-subsequent  (sig* #\+ #\- #\. #\@))
    (peculiar-identifier (alt (sig* #\+ #\-) (exact "...")))

    ; initial             -> <letter> | <special-initial>
    ; subsequent          -> <initial | <digit> | <special-subsequent>
    ; identifier          -> <initial> <subsequent>* | <peculiar-identifier>
    (initial             (alt letter special-initial))
    (subsequent          (alt initial digit special-subsequent))
    (identifier          (alt (seq initial (kln* subsequent))
                              peculiar-identifier))
    
    ; boolean        -> #t | #f
    ; character      -> #\ <any character> | #\ <character-name>
    ; character-name -> space | newline
    (boolean             (alt (exact-ci "#t") (exact-ci "#f")))
    (character-name      (alt (exact-ci "space") (exact-ci "newline")))
    (character
      (let ((hash-slash (seq (sig* #\#) (sig* #\\))))
        (alt (seq hash-slash (sig slex:alphanum))
             (seq hash-slash character-name))))
    
    ; string         -> " <string-element>* "
    ; string-element -> <any character other than " or \> | \" | \\
    (string-element (alt (sig*-co #\\ #\") (exact "\\\"") (exact "\\\\")))
    (string         (seq (sig* #\") (kln* string-element) (sig* #\")))
    
    (exponent-mark (sig* #\e #\s #\f #\d #\l #\E #\S #\F #\D #\L))
    (sign          (alt eps (sig* #\+ #\-)))
    (exactness     (alt eps (alt (exact "#i") (exact "#e"))))
    
    (digit-R       (list (cons 2  (sig* #\0 #\1))
                         (cons 8  (sig* #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))
                         (cons 10 digit)
                         (cons 16 (alt digit
                                       (sig*  #\a #\b #\c #\d #\e #\f
                                              #\A #\B #\C #\D #\E #\F)))))
    
    (radix-R       (list (cons 2  (exact-ci "#b"))
                         (cons 8  (exact-ci "#o"))
                         (cons 10 (alt eps (exact-ci "#d")))
                         (cons 16 (exact-ci "#x"))))
    
    (suffix
     (alt eps (seq exponent-mark sign (rep+ (cdr (assq 10 digit-R))))))
    
    (prefix-R
     (mapall R in '(2 8 10 16)
       (let ((radix (cdr (assq R radix-R))))
         (cons R
               (alt (seq radix exactness)
                    (seq exactness radix))))))
    
    (uinteger-R
      (mapall R in '(2 8 10 16)
        (cons R (seq (rep+ (cdr (assq R digit-R))) (exact "#*")))))
    
    (decimal
      (let ((uinteger (cdr (assq 10 uinteger-R)))
            (digit-10 (cdr (assq 10 digit-R))))
        (alt (seq uinteger suffix)
             (seq (sig* #\.) (rep+ digit-10) (exact "#*") suffix)
             (seq (rep+ digit-10) (sig* #\.) (kln* digit-10) (exact "#*") suffix)
             (seq (rep+ digit-10) (exact "#+") (sig* #\.) (exact "#*") suffix))))
    
    (ureal-R
      (mapall R in '(2 8 10 16)
        (let ((uinteger (cdr (assq R uinteger-R))))
          (cons R
                (alt uinteger
                     (seq uinteger (sig* #\/) uinteger)
                     decimal)))))
    
    (real-R
     (mapall R in '(2 8 10 16)
       (cons R (seq sign (cdr (assq R ureal-R))))))
    
    
    (complex-R
      (mapall R in '(2 8 10 16)
        (let ((the-i (exact-ci "i"))
              (real  (cdr (assq R real-R)))
              (ureal (cdr (assq R ureal-R))))
          (cons R
                (alt real
                  (seq real (sig* #\@) real)
                  (seq real (sig* #\+ #\-) ureal the-i)
                  (seq real (sig* #\+ #\-))
                  (seq (sig* #\+ #\-) ureal the-i)
                  (seq (sig* #\+ #\-) the-i))))))
    
    (num-R
     (mapall R in '(2 8 10 16)
       (cons R
         (seq (cdr (assq R prefix-R)) (cdr (assq R complex-R))))))
    
    ; number -> num-2 | num-8 | num-10 | num-16
    
    (num-2  (cdr (assq 2 num-R)))
    (num-8  (cdr (assq 8 num-R)))
    (num-10 (cdr (assq 10 num-R)))
    (num-16 (cdr (assq 16 num-R))))
  
  (rule
    (boolean      )
    (identifier   )
    (character    )
    (string       Lex/action:token-str)
    (num-2 )
    (num-8 )
    (num-10 )
    (num-16 )
    ((exact "#(") (cons 'vec-paren '()))
    ((exact ",@") (cons 'quasi-quote '()))
    ((sig* #\()   (cons 'left-paren '()))
    ((sig* #\))   (cons 'right-paren '()))
    ((sig* #\')   (cons 'quote '()))
    ((sig* #\`)   (cons 'quasi-quote '()))
    ((sig* #\,)   (cons '() '()))
    ((sig* #\.)   (cons 'dot '())))

;(define token num)

(define N (RE->NFA token))
(NFA->DOT N "test2.dot")

; (define N (RE->NFA token))
(define N2
  (@nanobench (NFA/eps-elimination N)))

(NFA->DOT N2 "NFA-elimination.dot")
(%exit)

;(define D (NFA->DFA N))

;(DFA->DOT (car D) "simple.dot")

;(forall pair in (cdr D)
;  (merge-edges! (car pair)))

;(DFA->DOT (car D) "merged.dot")
