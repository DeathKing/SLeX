;;; R5RS.scm -- a Scheme tokenizer under R5RS definition
;;;
;;; 
;;; Reference:
;;;   + http://www.schemers.org/Documents/Standards/R5RS/r5rs.pdf
;;;   + http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-10.html#%_chap_7

(load-relative "../slex.scm")

; digit               -> [0-9]
; letter              -> [a-zA-Z]
; special-initial     -> ! | $ | % | & | * | / | : | < | = | > | ? | ^ | _ | ~
; special-subsequent  -> + | - | . | @
; peculiar-identifier -> + | - | ...
; initial             -> <letter> | <special-initial>
; subsequent          -> <initial | <digit> | <special-subsequent>
; identifier          -> <initial> <subsequent>* | <peculiar-identifier>

(define digit  (sig slex:digit))
(define letter (sig slex:alpha))
(define special-initial (sig* #\! #\$ #\% #\& #\* #\/ #\: 
                              #\< #\= #\> #\? #\^ #\_ #\~))
(define special-subsequent  (sig* #\+ #\- #\. #\@))
(define peculiar-identifier (alt (sig* #\+ #\-) (exact "...")))

(define initial    (alt letter special-initial))
(define subsequent (alt initial digit special-subsequent))
(define identifier (alt (seq initial (kln* subsequent))
                        peculiar-identifier))

; boolean        -> #t | #f
; character      -> #\ <any character> | #\ <character-name>
; character-name -> space | newline
; string         -> " <string-element>* "
; string-element -> <any character other than " or \> | \" | \\
(define boolean (alt (exact-ci "#t") (exact-ci "#f")))
(define character-name (alt (exact-ci "space") (exact-ci "newline")))
(define character
  (let ((hash-slash (seq (sig* #\#) (sig* #\\))))
    (alt (seq hash-slash (sig slex:alphanum))
         (seq hash-slash character-name))))

; number -> num-2 | num-8 | num-10 | num-16

(define exponent-mark (sig* #\e #\s #\f #\d #\l #\E #\S #\F #\D #\L))
(define sign          (alt eps (sig* #\+ #\-)))
(define exactness     (alt eps (alt (exact "#i") (exact "#e"))))

(define digit-R
  (list (cons 2  (sig* #\0 #\1))
        (cons 8  (sig* #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))
        (cons 10 digit)
        (cons 16 (alt digit
                      (sig*  #\a #\b #\c #\d #\e #\f
                             #\A #\B #\C #\D #\E #\F)))))

(define radix-R
  (list (cons 2  (exact-ci "#b"))
        (cons 8  (exact-ci "#o"))
        (cons 10 (alt eps (exact-ci "#d")))
        (cons 16 (exact-ci "#x"))))

(define suffix (alt eps (seq exponent-mark sign (rep+ (cdr (assq 10 digit-R))))))

(define prefix-R
  (mapall R in '(2 8 10 16)
    (let ((radix (cdr (assq R radix-R))))
      (cons R
            (alt (seq radix exactness)
                 (seq exactness radix))))))

(define uinteger-R
  (mapall R in '(2 8 10 16)
    (cons R (seq (rep+ (cdr (assq R digit-R))) (exact "#*")))))

(define decimal
  (let ((uinteger (cdr (assq 10 uinteger-R)))
        (digit-10 (cdr (assq 10 digit-R))))
    (alt (seq uinteger suffix)
         (seq (sig* #\.) (rep+ digit-10) (exact "#*") suffix)
         (seq (rep+ digit-10) (sig* #\.) (kln* digit-10) (exact "#*") suffix)
         (seq (rep+ digit-10) (exact "#+") (sig* #\.) (exact "#*") suffix))))

(define ureal-R
  (mapall R in '(2 8 10 16)
    (let ((uinteger (cdr (assq R uinteger-R))))
      (cons R
            (alt uinteger
                 (seq uinteger (sig* #\/) uinteger)
                 decimal)))))

(define real-R
  (mapall R in '(2 8 10 16)
    (cons R (seq sign (cdr (assq R ureal-R))))))
  
(define complex-R
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

(define num-R
  (mapall R in '(2 8 10 16)
    (cons R
          (seq (cdr (assq R prefix-R)) (cdr (assq R complex-R))))))

(define num
  (apply alt (alist-get-values num-R)))
;(alt (cdr (assq 2 num-R))))


(define token  (alt boolean identifier character 
                    num
                    (sig* #\() (sig* #\)) (exact "#(") (sig* #\')
                    (sig* #\`) (sig* #\,)  (sig* #\.)
                    (exact ",@")
                    ) )

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




;(define alist (NFA/nodes->alist (car D)))
;(define fvec (NFA/alist->fvec alist))

;(define E (DFA/find-eqv-class (car D) alist fvec))

;(DFA/simplify! (car D))
;(DFA->DOT (car D) "simplified.dot")
;(format #t "simplified.dot done!~%")


;(define E (DFA/find-eqv-class (car D)))
;(define r (DFA/run (car D) "identifier"))

;(define r (RE/scan token "(define token  (alt boolean identifier character) )"))

;(define epsclos (NFA/eps-closure (caar (reverse alist))))

;(define (select-out epsclos alist)
;  (if (null? epsclos)
;      '()
;      (let ((entry (assq (car epsclos) alist)))
;        (if entry
;            (cons entry (select-out (cdr epsclos) alist))
;            (select-out (cdr epsclos))))))

;(pp (map cdr (select-out epsclos alist)))

;(define node1 (list-ref (reverse alist) 11))
;(define node2 (list-ref (reverse alist) 27))
;(define node3 (list-ref (reverse alist) 28))

;(define fwd
;    (NFA/forward-step (car node1) (caar (get-edges (car node3)))))

;(define t (NFA/nodes->serials fwd alist))

;(define d11-epsclos (NFA/eps-closure (car node1)))
;(define d11-epsclos-sr (NFA/nodes->serials d11-epsclos alist))

;(format #t "forward step is ~A" d11-epsclos-sr)

;(NFA->DOT N "test.dot")