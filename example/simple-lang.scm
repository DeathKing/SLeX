(load-relative "../slex.scm")

; Lex:
;     integer ::= <slex:digit> +
;  identifier ::= <slex:alpha> +
;        func ::= + | -
;   delimiter ::= <slex:whitespace>

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
        (lambda (token-str start-at length)
          (color-func token-str)))))
  
  (rule
    (integer     (sv-token string-white))
    (keyword-set (sv-token string-red))
    (identifier  (sv-token string-green))
    (func        (sv-token string-blue))
    (delimiter   Lex/action:token-str)
    (default     Lex/action:handle-error))) ;error-handling
  
;;; Grammar:
;;;     program ::= <stmt>*
;;;        stmt ::= (set! <identifier> <exp>) | <exp>
;;;         exp ::= <func-call> | <atom>
;;;   func-call ::= (<func> <exp> <exp>)
;;;        func ::= + | -
;;;        atom ::= <identifier> | <integer>

;(define match-atom

;	(let ((token (get-token! s0)))
;	  (if (is-atom? token)
;	      (cons 'atom token)
;	      (push-back! s0 token)))
;)

;(define match-func
;  (let ((token (get-token! s0)))
;    (if (is-func? token)
;        (cons 'func token)
;        (push-back! s0 token))))

;(define match-keyword-set
;  (let ((token (get-token! s0)))
;    (if (equal? token (cons 'keyword 'set!))
;        token
;        (push-back! s0 token))))

;(define match-func-call
;  (let ))

;(define eval)

(define s "12 34 asdfb set! + $ \nasdf 13")
(define L0 (Lex/instantiation simple-L s))

(until (LexInstance/eof? L0)
  (display (LexInstance/get-token! L0)))

