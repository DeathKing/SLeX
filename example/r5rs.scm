(load-relative "../slex.scm")

; digit               -> [0-9]
; letter              -> [a-z]
; special-initial     -> ! | $ | % | & | * | / | : | < | = | > | ? | ^ | _ | ~
; special-subsequent  -> + | - | . | @
; peculiar-identifier -> + | - | ...
; initial             -> <letter> | <special-initial>
; subsequent          -> <initial | <digit> | <special-subsequent>
; identifier          -> <initial> <subsequent>* | <peculiar-identifier>

(define digit  (sig #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(define letter (sig #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m 
                    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
(define special-initial (sig #\! #\$ #\% #\& #\* #\/ #\: 
                             #\< #\= #\> #\? #\^ #\_ #\~))
(define special-subsequent  (sig #\+ #\- #\. #\@))
(define peculiar-identifier (alt (sig #\+ #\-) (exact "...")))

(define initial    (alt letter special-initial))
(define subsequent (alt initial digit special-subsequent))
(define identifier (alt (seq initial (kleen subsequent))
                        peculiar-identifier))

(define alphanumerics
  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
    #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
    #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t
    #\u #\v #\w #\x #\y #\z
    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
    #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
    #\U #\V #\W #\X #\Y #\Z))

(define boolean (alt (exact-ci "#t") (exact-ci "#f")))
(define character-name (alt (exact-ci "space") (exact-ci "newline")))
(define character
  (let ((hash-slash (exact "#\\")))
    (alt (seq hash-slash (apply sig alphanumerics))
         (seq hash-slash character-name))))

;(define digit-R
;  (list (cons 2  (sig '(#\0 #\1)))
;        (cons 8  (sig '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)))
;        (cons 10 digit)
;        (cons 16 (alt digit
;                      (sig '(#\a #\b #\c #\d #\e #\f
;                             #\A #\B #\C #\D #\E #\F))))))

;(define radix-R
;  (list (cons 2  (exact "#b"))
;        (cons 8  (exact "#o"))
;        (cons 10 (alt eps (exact "#d")))
;        (cons 16 (exact "#x"))))

;(define exactness (alt eps (alt (exact "#i") (exact "#e"))))
;(define sign (alt eps
;                  (alt (sig #\+) (sig #\-))))



;(define N0 (RE->NFA subsequent))
;(define alist0 (NFA/nodes->alist N0))

;(NFA->DOT N0 "simple.dot")


(define N (RE->NFA identifier))
(NFA->DOT N "test2.dot")
;(define alist (NFA/nodes->alist N))

(define D (NFA->DFA N))
(DFA->DOT (car D) (cdr D) "simple.dot")
;(define r (run-DFA (car D) "identifier"))

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