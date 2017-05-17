
;;; join : str -> list<str> -> str
(define (join in ss)
  (reduce-left (lambda (x y) (string-append x in y)) "" ss))