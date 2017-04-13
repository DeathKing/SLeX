```text
               ;;;;;;;;                    ;;;;  ;;;; 
                  ;;                        ;;    ;;  
                  ;;                         ;;  ;;   
     ;;;;;;;;     ;;                         ;;; ;    
    ;;     ;;     ;;                          ;;;     
    ;;            ;;                           ;;     
    ;;;;;;;;      ;;      ;;                  ;;;;    
      ;;;;;;;     ;;      ;;   ;;;;;;;;      ;;  ;    
    ;      ;;     ;;      ;;  ;;     ;;     ;;    ;   
    ;;;    ;;     ;;      ;; ;;       ;;    ;     ;;  
    ;;;;;;;;;  ;;;;;;;;;;;;; ;;;;;;;;;;;  ;;;;;  ;;;;;
                             ;;           
                              ;;     ;;;  
                              ;;;;;;;;;
```

Yet another lex analysor generator written in programming language Scheme.

## usage

### basic regular expression and combinators

`sig` matches exactly one char in canditate chars.

```scheme
(define digit (sig #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(RE-matches? digit "1")    ; ==> #t
(RE-matches? digit "a")    ; ==> #f
(RE-matches? digit "12")   ; ==> #f
```

`exact` and `exact-ci` matches exactly the string given, while `exact-ci` is case-insensitive:

```scheme
(define switch0 (exact "switch0"))
(define switch0-ci (exact-ci "switch0"))

(RE-matches? switch0 "switch0")     ; ==> #t
(RE-matches? switch0 "SWITCH0")     ; ==> #f
(RE-matches? switch0-ci "sWiTCh0")  ; ==> #t
```

`alt` connect two RE:

`seq`

`kleen`


