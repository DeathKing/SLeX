;;; UTIL.scm -- some useful syntax patch

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
;;;    p and p0 must be a one argument procedure, (p e) will be evaluated
;;;    at first, then pass it as argument to p0 to further evaluation, and
;;;    so on.
;;;
;;;              call -> : T -> (T -> U) -> U
;;;             apply ~> : List<T> -> (T* -> U) -> U
;;;               map => : List<T> -> (T -> U) -> List<U>
;;;            filter %> : List<T> -> (T -> Boolean) -> List<T>
(define-syntax then
  (syntax-rules (-> => ~> %>)
    ((_ e -> p)  (p e))
    ((_ e => p)  (map p e))
    ((_ e %> p)  (filter p e))
    ((_ e ~> p)  (apply p e))
    ((_ e v p exps ...)
     (then (then e v p) exps ...))))

(define-syntax type-check
  (syntax-rules ()
    ((_ object pred in-proc)
     (if (not (pred object))
         (error:wrong-type-argument object '() in-proc)))
    ((_ object pred)
     (if (not (pred object))
         (error:wrong-type-argument object '() '())))))