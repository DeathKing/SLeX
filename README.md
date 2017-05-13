```text
          .M"""bgd `7MMF'               `YMM'   `MP' 
         ,MI    "Y   MM                   VMb.  ,P   
         `MMb.       MM                    `MM.M'    
           `YMMNq.   MM                      MMb     
         .     `MM   MM      ,  .gP"Ya     ,M'`Mb.   
         Mb     dM   MM     ,M ,M'   Yb   ,P   `MM.  
         P"Ybmmd"  .JMMmmmmMMM 8M"""""" .MM:.  .:MMa.
                               YM.    ,
                                `Mbmmd'
``` 

Yet another lex analysor generator written in programming language Scheme.

**Warning!** This project is still evolving, some API may change later. 

## usage

### primitives and combinators

The constant `eps` returns a `epsilon`-RE which matches whatever you gave:

```scheme
(RE/matches? eps "")     ; ==> #t
(RE/matches? eps "asdf") ; ==> #t
```

Gavin a char-set, `sig` construct a RE that matches exact one char in that candidate set. On the other hand, `sig*` receives multiple chars as arguments and packed them as a char-set.

```scheme
(define digit (sig slex:digit))
(define digit (sig* #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(RE/matches? digit "1")    ; ==> #t
(RE/matches? digit "a")    ; ==> #f
(RE/matches? digit "12")   ; ==> #f
```

There are some pre-defined char-sets that you can use:

```scheme
(define slex:digit       char-set:numeric)
(define slex:upper-case  char-set:upper-case)
(define slex:lower-case  char-set:lower-case)
(define slex:alpha       char-set:alphabetic)
(define slex:alphanum    char-set:alphanumeric)
(define slex:whitespace  char-set:whitespace)
(define slex:standard    char-set:standard)
(define slex:graphic     char-set:graphic)
```

`exact` and `exact-ci` match exactly the given string, while `exact-ci` means case-insensitive:

```scheme
(define switch0 (exact "switch0"))
(define switch0-ci (exact-ci "switch0"))

(RE/matches? switch0 "switch0")     ; ==> #t
(RE/matches? switch0 "SWITCH0")     ; ==> #f
(RE/matches? switch0-ci "sWiTCh0")  ; ==> #t
```

`alt` makes an alternation of two or more REs:

```scheme
(define peculiar-identifier (alt (sig* #\+ #\-) (exact "..."))

(RE/matches? peculiar-identifier "+")     ; ==> #t
(RE/matches? peculiar-identifier "...")   ; ==> #t
(RE/matches? peculiar-identifier "+...")  ; ==> #f 
```

`seq` connects two or more REs:

```scheme
; equivalent to (exact "...")
(define ldots (seq (sig* #\.) (sig* #\.) (sig* #\.))) 

(RE/matches? ldots "...")   ; ==> #t
(RE/matches? ldots ".")     ; ==> #f
(RE/matches? ldots "....")  ; ==> #f
```

`rep?` means that the RE should appear zero or exact one time:

```scheme
; positive-integer may have a plus sign prefix
(define positive-integer-revised (seq (rep? (sig* #\+)) positive-integer))

(RE/matches? positive-integer-revised "+012345")   ; ===> #t
```

`kln*` creates a so called **Kleene Closure** of a RE:

```scheme
; for simplicity, we suppose that integer could start with '0'
(define positive-integer (kln* slex:digit))

(RE/matches? positive-integer "123456")   ; ===> #t
(RE/matches? positive-integer "012345")   ; ===> #t
(RE/matches? positive-integer "0.1234")   ; ===> #f
``` 

### use RE to match pattern

`


### use RE to scan pattern

### use RE to substitute pattern

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

## Roadmap

```text
 ~~~~~~~~~~~~~
   ^      String                (DFANode . List<DFANode>)
   |       +\----+                    /
  TODO     |  RE |                 DFA
   |       +--+--+                  ^
   v    Parse |                     | NFA->DFA
 ~~~~~~~~~~~~ v                     | 
            RE IR ---------------> NFA
             /        RE->NFA         \
    (Symbol . Char-set | RE)   (NFANode . NFANode)        

```
