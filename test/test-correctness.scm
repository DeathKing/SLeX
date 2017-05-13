(load-relative "../slex.scm")

; sign       ::= + | - | \epsilon
; high-digit ::= 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
; natural    ::= <high-dight><digit>* | <digit>
; float      ::= .<digit>+ | \epsilon
; number     ::= <sign><natural><float>

(let* ((sign       (alt (sig* #\+ #\-) eps))
       (digit      (sig slex:digit))
       (high-digit (sig* #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
       (natural    (alt (seq high-digit (kln+ digit)) digit))
       (float      (alt (seq (sig* #\.) (kln+ digit)) eps))
       (number     (seq sign natural float)))
  
    (run-test-with-cases
      (list "1.1")
      (it should `(RE/matches? number ,_))
      )
    
    (run-test-with-cases
      (list "+.32" "1." "012.123" "12.1+")
      (it should-not `(RE/matches? number ,_)))
)