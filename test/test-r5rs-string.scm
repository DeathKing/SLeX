(load-relative "../slex.scm")

(define r5rs-string-element
  (alt (sig*-co #\\ #\") (exact "\\\"") (exact "\\\\")))

(define r5rs-string
  (seq (sig* #\") (kln* r5rs-string-element) (sig* #\")))

(define r5rs-string-re (RE/compile r5rs-string))

(RE/matches? r5rs-string-re "\"../slex.scm\"")