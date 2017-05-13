

; Lex:
;     integer ::= <slex:digit> +
;  identifier ::= <slex:letter> +
;        func ::= + | -
;   delimiter ::= <slex:whitespace>

(define-lex simple-L
  (definition
    (integer     (kln+ (sig slex:digit)))
    (identifier  (kln+ (sig slex:letter)))
    (func        (sig* #\+ #\-))
    (keyword-set (exact-ci "set!"))
    (delimiter   (sig slex:whitespace)))
  
  (rule
    (integer     string->symbol)
    (identifier  string->symbol)
    (keyword-set (cons 'keyword 'set!))
    (delimiter   ignore)
    (else        error-handling)))
  
;;; Grammar:
;;;     program ::= <stmt>*
;;;        stmt ::= (set! <identifier> <exp>) | <exp>
;;;         exp ::= <func-call> | <atom>
;;;   func-call ::= (<func> <exp> <exp>)
;;;        func ::= + | -
;;;        atom ::= <identifier> | <integer>

(define match-atom

	(let ((token (get-token! s0)))
	  (if (is-atom? token)
	      (cons 'atom token)
	      (push-back! s0 token)))
)

(define match-func
  (let ((token (get-token! s0)))
    (if (is-func? token)
        (cons 'func token)
        (push-back! s0 token))))

(define match-keyword-set
  (let ((token (get-token! s0)))
    (if (equal? token (cons 'keyword 'set!))
        token
        (push-back! s0 token))))

(define match-func-call
  (let ))

(define eval)


