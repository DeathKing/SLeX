;;; TIGER.scm -- a Tiger language tokenizer
;;;
;;; Reference:
;;;   + https://www.lrde.epita.fr/~tiger/tiger.html
;;;   + https://www.lrde.epita.fr/~tiger//tiger.pdf

;;;        letter ::= <slex:alphabet>
;;;         digit ::= <slex:digit>
;;; subsequent-id ::= <digit> | <letter> | _
;;;            id ::= <letter><subsequent-id>*
;;;       keyword ::= while | for | to | break | let | in | end |
;;;                   function | var | type | array | if | then |
;;;                   else |do | of | nil
;;;   punctuation ::= , | : | ; | ( | ) | [ | ] | { | } | . | + | - |
;;;                   * | / | = | <> | < | <= | > | >= | & | \ | :=

(define-lex tiger-L
  (definition
    (letter        (sig* slex:alphabet))
    (digit         (sig* slex:digit))
    (punctuation   (apply alt
                          (append (list (sig* #\, #\: #\; #\( #\) #\[ #\] #\{
                                              #\} #\. #\+ #\- #\+ #\/ #\= #\>
                                              #\> #\& #\\))
                                  (map exact '("<>" "<=" ">=" ":=")))))
    (subsequent-id (alt digit letter (sig* #\_)))
    (keyword       (apply alt (map exact 
                                   '("while" "for" "to" "break" "let" "in"
                                     "end" "var" "function" "type" "array"
                                     "if" "then" "else" "do" "of" "nil"))))
    (id            (seq letter (kleen subsequent-id))))
  (rule
    (id
     (lambda (s) (cons 'id s)))
    (punctuation
     (lambda (s) (cons 'punctuation (string->symbol s))))))

(rules ...) =>
(let ((M (new-empty-lex)))
  (LeX/add-rule! id proc)
  (LeX/finalized! M))