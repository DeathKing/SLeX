```text
    ;;;;;;;;  ;;;;;;;;                    ;;;;  ;;;; 
   ;;;    ;;     ;;                        ;;    ;;  
   ;;      ;     ;;                         ;;  ;;   
   ;;            ;;                         ;;; ;    
    ;;           ;;                          ;;;     
      ;;;;;      ;;                           ;;     
          ;;     ;;      ;;                  ;;;;    
   ;;      ;;    ;;      ;;   ;;;;;;;;      ;;  ;    
   ;;      ;;    ;;      ;;  ;;     ;;     ;;    ;   
   ;;;    ;;;    ;;      ;; ;;       ;;    ;     ;;  
     ;;;;;;;  ;;;;;;;;;;;;; ;;;;;;;;;;;  ;;;;;  ;;;;;
                             ;;           
                              ;;     ;;;  
                              ;;;;;;;;;
``` 

Yet another lex analysor generator written in programming language Scheme.

## usage

### primitives and combinators

The constant `eps` returns a `epsilon`-RE which matches whatever you gave:

```scheme
(RE-matches? eps "")     ; ==> #t
(RE-matches? eps "asdf") ; ==> #t
```

Gavin a set of chars, `sig` construct a RE that matches exact one char in that candidate set.

```scheme
(define digit (sig #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(RE-matches? digit "1")    ; ==> #t
(RE-matches? digit "a")    ; ==> #f
(RE-matches? digit "12")   ; ==> #f
```

There are some pre-defined chars set (which is implemented as a Scheme list) you could use:

```scheme
(define slex:digit (map digit->char (iota 10))
(define slex:upper-case (map (ascii->char (iota 26 (char->ascii #\A)))))
(define slex:lower-case (map (ascii->char (iota 26 (char->ascii #\a)))))
(define slex:alpha (append slex:upper-case slex:lower-case))
(define slex:alphanum (append slex:digit slex:alpha))
(define slex:whitespace (list #\space #\tab #\page #\linefeed #\return))
(define slex:standard (append slex:alphanum
  (list #\! #\" #\# #\$ #\% #\& #\’ #\( #\) #\* #\+ #\, #\- #\. #\/ #\: #\; #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^ #\_ #\‘ #\{ #\| #\} #\~)))
(define slex:graphic `(,@slex:standard #\space))
```

`exact` and `exact-ci` matches exactly the string given, while `exact-ci` is case-insensitive:

```scheme
(define switch0 (exact "switch0"))
(define switch0-ci (exact-ci "switch0"))

(RE-matches? switch0 "switch0")     ; ==> #t
(RE-matches? switch0 "SWITCH0")     ; ==> #f
(RE-matches? switch0-ci "sWiTCh0")  ; ==> #t
```

`alt` makes an alternation of two or more REs:

```scheme
(define peculiar-identifier (alt (sig #\+ #\-) (exact "..."))

(RE-matches? peculiar-identifier "+")     ; ==> #t
(RE-matches? peculiar-identifier "...")   ; ==> #t
(RE-matches? peculiar-identifier "+...")  ; ==> #f 
```

`seq` conect two or more REs:

```scheme
; equivalent to (exact "...")
(define ldots (seq (sig #\.) (sig #\.) (sig #\.))) 

(RE-matches? ldots "...")   ; ==> #t
(RE-matches? ldots ".")     ; ==> #f
(RE-matches? ldots "....")  ; ==> #f
```

`kleen` creates a so called **Kleen Closure** of a RE:

```scheme
; for simplicity, we suppose that integer could start with '0'
(define positive-integer (kleen slex:digit))

(RE-matches? positive-integer “123456”)   ; ===> #t
(RE-matches? positive-integer “012345”)   ; ===> #t
(RE-matches? positive-integer “0.1234”)   ; ===> #f
``` 

### matching RE

a

### token analysis

To analysis the token stream, you should define a lexer with `define-lex` special form and add rules to it:

```scheme
(define-lex S
  ((sig #\( #\) #\+ #\-) id)
  ((exact-ci "set!") (cons 'keyword 'set!)
  ((kleen+ slex:letter) string->symbol)
  ((kleen+ slex:digit) string->number)
  ((apply sig slex:whitespace) ignore)
  (else error-handling))
```


```scheme
(define s0 (S standard-input))

(define exp-streem (make-parse-stream (get-lex-stream s0)))

(let loop ((sigma `((+ . ,fx+) (- . ,fx-))
           (exp exp-streem))
  (if (null? exp)
      '()
      (loop (eval (head exp) sigma)
            (tail exp))))
```

```text
Lex:
integer ::= <slex:digit> +
identifier ::= <slex:letter> +
delimiter ::= <slex:whitespace>

Grammar
program ::= <stmt>*
stmt ::= (set! <identifier> <exp>) | <exp>
exp ::= <func-call> | <atom>
func-call ::= (<func> <exp> <exp>)
func ::= + | -
atom ::= <identifier> | <integer>
```

