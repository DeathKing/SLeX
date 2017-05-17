;;; LIST.scm -- some auxiliary function for list structure.

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
              (cons (cons (cdar lst) (caar lst)) result)))))

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
