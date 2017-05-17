
(define (make-mirror-table width height ini-val)
  (make-initialized-vector height
    (lambda (_)
      (make-vector width ini-val))))

(define (mirror-table-put! table x y val)
  (vector-set! (vector-ref table y) x val)
  (vector-set! (vector-ref table x) y val))

(define (mirror-table-get table x y)
  (vector-ref (vector-ref table x) y))
