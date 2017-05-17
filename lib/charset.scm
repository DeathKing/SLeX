
(define char-set:empty (string->char-set ""))
(define char-set:universe (char-set-invert char-set:empty))

(define (empty-char-set? cs)
  (and (char-set? cs)
       (null? (char-set->scalar-values cs))))

(define (universe-char-set? cs)
  (and (char-set? cs)
       (empty-char-set? (char-set-invert cs))))
