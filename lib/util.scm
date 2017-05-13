(define-syntax type-check
  (syntax-rules ()
    ((_ object pred in-proc)
     (if (not (pred object))
         (error:wrong-type-argument object '() in-proc)))
    ((_ object pred)
     (if (not (pred object))
         (error:wrong-type-argument object '() '())))))