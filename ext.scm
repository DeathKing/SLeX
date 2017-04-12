;;; forall
;;;
;;; This is a useful syntax wrap for for-each function while writing some loop-
;;; based algorithms.
;;;
;;; usage:  (forall <v> <in|of> <s> <exp>+)
;;;
;;;       <v> must be a free identifier which may occur in <exp>+;
;;;       <s> must be a expression evaluated to a list;
;;;   <in|of> if the keyword in is specified, then for-each procedure will be
;;;           used, otherwise use the map procedure;
;;;    <exp>+ are the expressions you want to evaluated under for every time
;;;           <v> binds to a element in <s>;
;;;
(define-syntax forall
  (syntax-rules (in)
    ((_ v in s body ...)
     (for-each (lambda (v) body ...) s))))

(define-syntax mapall
  (syntax-rules (in)
    ((_ v in s  body ...)
     (map (lambda (v) body ...) s))))

;;; then
;;;
;;; usage: (then e -> p [-> p0]*)
;;;
;;;   p and p0 must be a one argument procedure, (p e) will be evaluated
;;;   at first, then pass it as argument to p0 to further evaluation, and
;;;   so on.
(define-syntax then
  (syntax-rules (->)
    ((_ e -> p)
     (p e))
    ((_ e -> p1 -> p2 ...)
     (then (p1 e) -> p2 ...))))

(define (list-unique lst)
  (if (null? lst)
      '()
      (let ((first (car lst)) (rest (cdr lst)))
        (if (member first rest)
            (list-unique rest)
            (cons first (list-unique rest))))))

(define (list-flatten lst)
  (if (not (list? lst))
      (list lst)
      (fold-left append '() (map list-flatten lst))))

(define (list-compact lst)
  (filter (lambda (e) (not (null? e))) lst))

(define (list-join lst in)
  (reduce-left (lambda (x y) (append x in y)) '() lst))

;;; join : str -> list<str> -> str
(define (join in ss)
  (reduce-left (lambda (x y) (string-append x in y)) "" ss))

(define rassq (association-procedure eq? cdr))
(define rassv (association-procedure eqv? cdr))
(define rassoc (association-procedure equal? cdr))
