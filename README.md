<p align="center">
  <img width="90%" src="./assets/logo.png" alt="SLeX Logo">
</p>

Yet another not-so-simple lex analysor generator and naïve regular expression engine written in programming language Scheme.

**Warning!** This project is still evolving, most of the API will be changed later. 

## Use SLeX as Regular expression engine

A [regular expression](https://en.wikipedia.org/wiki/Regular_expression) usually is a sequence of characters that define a search pattern.

In usual programming language, Regular Expression is defined in form of String or literal. But for now in SLeX, you can only use RE-IR (Intermediate Representation) to define a RE (see [Roadmap](#roadmap)). In SLeX, we have:

  + 1 primitive RE-IR constant:
    1. `eps`: eat none of input characters.
  + 6 primitive constructors: 
    1. `sig`, `sig*`: eat a single character in the given char-set.
    2. `sig-co`, `sig*-co`: eat any single character which not in the given char-set.
    3. `exact`: match exactly the given string (i.e. sequence of chars), case-sensitively.
    4. `exact-ci`: match exactly the given string, but case-insensitively.
 + and another 5 RE-IR combinators:
    1. `alt`
    2. `seq`
    3. `rep?`
    4. `rep+`
    5. `kln*`

### primitives and combinators

The constant `eps` returns a ε-RE which matches whatever you gave:

```scheme
(define RE-eps (RE/compile eps))

(RE/matches? RE-eps "")     ; ==> #t
(RE/matches? RE-eps "asdf") ; ==> #t
```

Gavin a char-set, `sig` construct a RE that matches exact one char in that candidate set. On the other hand, `sig*` receives multiple chars as arguments and packed them as a char-set.

```scheme
(define RE-digit (RE/compile (sig slex:digit)))
(define RE-digit (RE/compile (sig* #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

(RE/matches? RE-digit "1")    ; ==> #t
(RE/matches? RE-digit "a")    ; ==> #f
(RE/matches? RE-digit "12")   ; ==> #f
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
(define RE-switch0 (RE/compile (exact "switch0")))
(define RE-switch0-ci (RE/compile (exact-ci "switch0")))

(RE/matches? RE-switch0 "switch0")     ; ==> #t
(RE/matches? RE-switch0 "SWITCH0")     ; ==> #f
(RE/matches? RE-switch0-ci "sWiTCh0")  ; ==> #t
```

`alt` makes an alternation of two or more REs:

```scheme
(define RE-peculiar-identifier (RE/compile (alt (sig* #\+ #\-) (exact "...")))

(RE/matches? RE-peculiar-identifier "+")     ; ==> #t
(RE/matches? RE-peculiar-identifier "...")   ; ==> #t
(RE/matches? RE-peculiar-identifier "+..")   ; ==> #f 
```

`seq` connects two or more REs:

```scheme
; equivalent to (exact "...")
(define RE-ldots (RE/compile (seq (sig* #\.) (sig* #\.) (sig* #\.))))

(RE/matches? RE-ldots "...")   ; ==> #t
(RE/matches? RE-ldots ".")     ; ==> #f
(RE/matches? RE-ldots "....")  ; ==> #f
```

`rep?` means that the RE should appear zero or exact one time:

```scheme
; positive-integer may have a plus sign prefix
(define RE-positive-integer-revised
        (RE/compile (seq (rep? (sig* #\+)) positive-integer)))

(RE/matches? RE-positive-integer-revised "+012345")   ; ===> #t
```

`kln*` creates a so called **Kleene Closure** of a RE:

```scheme
; for simplicity, we suppose that integer could start with '0'
(define RE-positive-integer (RE/compile (kln* slex:digit)))

(RE/matches? RE-positive-integer "123456")   ; ===> #t
(RE/matches? RE-positive-integer "012345")   ; ===> #t
(RE/matches? RE-positive-integer "0.1234")   ; ===> #f
``` 

### use RE to match pattern

if a RE r is equivalent to a DFA M, thus, a String s ∈ L(r) iff δ q0 s ∈ F or partial-δ q0 s "" = f s "".  

### use RE to scan pattern

`scan` function will return all the possible matches occures in text. 

### use RE to substitute pattern

## Use SLeX to generate token analysor

To analysis the token stream, you should define a lexer with `define-lex` special form and then add definitions and rules to it. The `define-lex` special form has following forms:

```scheme
(define-lex <lex-id> {<definition-exps>} <rule-exps>)

;;; where
;;;   <definition-exps> ::= (<def-type> <def-clause> ... )
;;;   <def-type>    ::= definitions | definitions*
;;;   <def-clause>  ::= (<identifier> <scheme-exp>)
;;;
;;;   <rule-exps>   ::= (rule <rule-clause> ... {<default-exp>})
;;;   <rule-clause> ::= (<pattern> <action>)
;;;   <pattern>     ::= <scheme-exp>
;;;   <action>      ::= <scheme-exp>
;;;   <default-exp> ::= (default <scheme-exp>)
```

The semantic of `define-lex` could be described as:

> the special form `define-lex` defines a named lexer under a lexical enviornment expanded by keyword `definition` (using `let`) or `definitions` (using `let*`), whose pattern-action pairs are specified by keyword `rule`, where the `<pattern>` of `<rule-clause>` must be a expression which evaluated to be a RE-IR or RE-string, and the `<action>` of`<rule-clause>` must be a expression which evaluated to be a `self-evaluating` data or a tripple arugments procedure. 

This may be confusing, let's take `example/simple-lang.scm` as example:

```scheme
(define-lex simple-L
  (definition
    (integer     (rep+ (sig slex:digit)))
    (identifier  (rep+ (sig slex:alpha)))
    (func        (sig* #\+ #\-))
    (keyword-set (exact-ci "set!"))
    (delimiter   (sig slex:whitespace))
    
    ; also action procedure    
    (sv-token
      (lambda (color-func)
        (lambda (token-str start-at)
          (color-func token-str)))))
  
  (rule
    (integer     (sv-token string-white))
    (keyword-set (sv-token string-red))
    (identifier  (sv-token string-green))
    (func        (sv-token string-blue))
    (delimiter   Lex/action:token-str)
    (default     Lex/action:handle-error)))
```


## Theoretical detail of SLeX

### Roadmap

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

A DFA M is a 5-tuple, (Q, Σ, δ, q0, F), consisting of

  + a finite set of states (Q)
  + a finite set of input symbols called the alphabet (Σ)
  + a transition function (δ : Q × Σ → Q)
  + an initial or start state (q0 ∈ Q)
  + a set of accept states (F ⊆ Q)

It is very useful to extend the transition function to receive a string as argument, thus we may define δ' as (where `$` stands for end-of-string):

```haskell
δ' : Q x Σ* -> Q
δ' q $ = q
δ' q (w :: ws) = δ (δ' q ws) w
```

Sometime, when defining a `scan` like function, we may find a partial transition function is useful also. A partial transition function also accept two argument as the normal transition function:

```haskell
partial-δ Q x Σ* x Σ* -> Σ* x Σ*
partial-δ q xs $ = xs :: ""
partial-δ q xs (w :: ws) =
    if δ q w == fail then xs :: (w :: ws)
    else partial-δ (δ q w) (append xs w) ws
```

