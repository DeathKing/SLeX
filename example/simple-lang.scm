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
    (operator    (sig* #\+ #\- #\* #\/))
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
    (operator    (sv-token string-blue))
    ((sig* #\()  (sv-token string-red))
    ((sig* #\))  (sv-token string-red))
    (delimiter   Lex/action:token-str)
    (default     Lex/action:handle-error))) ;error-handling
  
;;; Grammar:
;;;     program ::= <stmt>*
;;;        stmt ::= (set! <identifier> <exp>) | <exp>
;;;         exp ::= <func-call> | <atom>
;;;   func-call ::= (<operator> <exp> <exp>)
;;;    operator ::= + | - | * | /
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

(define s "(set! a 1)
(set! b (* (+ 3 4) (- 7 9)))")

(define L0 (Lex/instantiation simple-L s))

(until (LexInstance/eof? L0)
  (display (LexInstance/get-token! L0)))

(%exit)