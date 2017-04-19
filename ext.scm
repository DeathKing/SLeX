;;; SLeX - ext.scm
;;;
;;;   This file contains some useful syntax extension
;;;
;;; DeathKing <dk@hit.edu.cn>
;;; 2017-04-08

;;; forall
;;;
;;; This is a useful syntax wrap for for-each function while writing some loop-
;;; based algorithms.
;;;
;;; usage:  (forall <binding-exp> in <list-exp> <exp>+), where:
;;;
;;;       <binding-exp> :: <identifier> |
;;;                        (<identifier> <identifier> <identifier>)
;;;
;;;   <list-exp>   must be a expression evaluated to a list;
;;;       <exp>+   are the expressions you want to evaluated under for every time
;;;
(define-syntax forall
  (syntax-rules (in)
    ((_ (v c b) in s body ...)
     (let ((val s))
       (call-with-current-continuation
       (lambda (b)
         ((cond ((list? val) foreach-k)
                ((vector? val) vector-foreach-k)
                (else (error "only list or vector are supported!")))
          (lambda (v c) body ...) val)))))
    ((_ v in s body ...)
     (let ((val s))
       ((cond ((list? val) for-each)
              ((vector? val) vector-for-each)
              (else (error "only list or vector are supported!")))
        (lambda (v) body ...) val)))))

;;; foreach-k : (U -> K) -> List<U> -> #!unspecific
(define (foreach-k proc lst)
  (if (null? lst)
      #!unspecific
      (begin
        (call-with-current-continuation
          (lambda (k)
            (proc (car lst) k)))
        (foreach-k proc (cdr lst)))))

(define (vector-foreach-k proc vec)
  (let ((bound (vector-length vec)))
    (let loop ((ref 0))
      (if (= ref bound)
          #!unspecific
          (begin
            (call-with-current-continuation
              (lambda (k)
                (proc (vector-ref vec ref) k)))
            (iter (+ 1 ref)))))))

(define-syntax mapall
  (syntax-rules (in)
    ((_ v in s body ...)
     (map (lambda (v) body ...) s))))

(define-syntax while
  (syntax-rules ()
    ((_ test body ...)
     (let loop ()
       (if test
           (begin body ... (loop)))))))

(define-syntax until
  (syntax-rules ()
    ((_ test body ...)
     (while (not test) body ...))))

;;; then
;;;
;;; usage: (then e <exp>+) , where:
;;;
;;;       <exp> ::= <arrow> <identifier>
;;;       <arrow> ::= -> | ~> | => | %>
;;;
;;;   p and p0 must be a one argument procedure, (p e) will be evaluated
;;;   at first, then pass it as argument to p0 to further evaluation, and
;;;   so on.

;;;   call -> : T -> (T -> U) -> U
;;;  apply ~> : List<T> -> (T* -> U) -> U
;;;    map => : List<T> -> (T -> U) -> List<U>
;;; filter %> : List<T> -> (T -> Boolean) -> List<T>

(define-syntax then
  (syntax-rules (-> => ~> %>)
    ((_ e -> p)  (p e))
    ((_ e => p)  (map p e))
    ((_ e %> p)  (filter p e))
    ((_ e ~> p)  (apply p e))
    ((_ e v p exps ...)
     (then (then e v p) exps ...))))

(define (list-uniq lst)
  (if (null? lst)
      '()
      (let ((first (car lst)) (rest (cdr lst)))
        (if (memq first rest)
            (list-uniq rest)
            (cons first (list-uniq rest))))))

(define (list-uniqv lst)
  (if (null? lst)
      '()
      (let ((first (car lst)) (rest (cdr lst)))
        (if (memv first rest)
            (list-uniqv rest)
            (cons first (list-uniqv rest))))))

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

(define (all? proc lst)
  (cond ((null? lst)
         (error "the 2nd argument should not be null!"))
        ((null? (cdr lst))
         (proc (car lst)))
        (else
         (and (proc (car lst)) (all? proc (cdr lst))))))

(define rassq (association-procedure eq? cdr))
(define rassv (association-procedure eqv? cdr))
(define rassoc (association-procedure equal? cdr))


; assq-entries : List<T> -> List<(T . U)> -> List<(T . U)>
(define (assq-entries items alist)
  (map (lambda (x) (assq x alist)) items))

; assq-values : List<T> -> List<(T . U)> -> List<U>
(define (assq-values items alist #!optional default)
  (let ((result (assq-entries items alist)))
    (if (and (not (null? result)) (all? (lambda (x) x) result))
        (map cdr result)
        (or (and (default-object? default) default) '()))))

; alist-reverse-all-pairs : List<(T . U)> -> List<(U . T)>
(define (alist-reverse-all-pairs alist)
  (let iter ((lst alist) (result '()))
    (if (null? lst)
        result
        (iter (cdr lst)
              (cons (cons (cdr (car lst)) (car (car lst))) result)))))

; alist-merge : List<(T . U)> -> (U -> U -> U) -> List<(T . U)>
(define (alist-merge alist proc)
  (let iter ((result '()) (open alist))
    (cond ((null? open) result)
          ((memq (caar open) result) =>
           (lambda (entry)
             (set-cdr! entry (proc (cdr entry) (cdar open)))
             (iter result (cdr open))))
          (else
           (iter (cons (car open) result) (cdr open))))))

(define (alist-get-keys alist) (map car alist))
(define (alist-get-values alist) (map cdr alist))


;;; join : str -> list<str> -> str
(define (join in ss)
  (reduce-left (lambda (x y) (string-append x in y)) "" ss))


(define (make-2D-mirror-table width height ini-val)
  (make-initialized-vector height
    (lambda (_)
      (make-vector width ini-val))))

(define (mirror-table-put! table x y val)
  (vector-set! (vector-ref table y) x val)
  (vector-set! (vector-ref table x) y val))

(define (mirror-table-get table x y)
  (vector-ref (vector-ref table x) y))

(define char-set:empty (string->char-set ""))
(define char-set:universe (char-set-invert char-set:empty))

(define (empty-char-set? cs)
  (and (char-set? cs)
       (null? (char-set->scalar-values cs))))

(define (universe-char-set? cs)
  (and (char-set? cs)
       (empty-char-set? (char-set-invert cs))))
