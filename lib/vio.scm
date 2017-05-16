;;; VIO.scm -- a vector implementation of IO stream.
(load-option 'format)
(load-relative "color.scm")

(define (make-vio #!optional cont)
  (let* ((cont (if (default-object? cont) "" cont))
         (vec (list->vector (string->list cont))))
    (cons 'vio
          (list
            (cons 'offset 0)
            (cons 'length (vector-length vec))
            (cons 'total (vector-length vec))
            (cons 'content vec)))))

(define VIO/default-rello-algo
  (let ((scale-facotr 3)) ;1.7321
    (lambda (origin-length pad-length)
      (+ origin-length (inexact->exact (round (* scale-facotr pad-length)))))))

(define (VIO/fill-in! vio str #!optional rellocation-algo)
  (let* ((origin-length (VIO/get-length vio))
         (pad-length (string-length str))
         (free (- (VIO/get-attr vio 'total) origin-length))
         (cont (VIO/get-content vio))
         (vstr (list->vector (string->list str)))
         (algo (or (and (default-object? rellocation-algo) VIO/default-rello-algo)
                   rellocation-algo)))
    (if (>= free pad-length)
        (begin
          (subvector-move-left! vstr 0 pad-length cont origin-length)
          (VIO/set-attr! vio 'length (+ origin-length pad-length)))
        (let* ((new-total (algo origin-length pad-length))
               (new-content  (vector-grow cont new-total)))
          (subvector-move-left! vstr 0 pad-length new-content origin-length)
          (VIO/set-attr! vio 'total new-total)
          (VIO/set-attr! vio 'content new-content)
          (VIO/set-attr! vio 'length (+ origin-length pad-length))))))

(define (VIO/inspect vio #!optional line-width)
  (let ((line-width (or (and (default-object? line-width) 60) line-width))
        (cont (VIO/get-content vio))
        (offset (VIO/tell vio))
        (total (VIO/get-attr vio 'total))
        (slen (VIO/get-length vio)))
    (format #t "** VIO/inspect **~%")
    (format #t "--------------------------------------------------~%")
    (format #t " offset: ~s~%" offset)
    (format #t " length: ~s~%" slen)
    (format #t "  total: ~s~%" total)
    (format #t "--------------------------------------------------")
    (let iter ((index 0))
      (if (equal? 0 (modulo index line-width))
          (format #t "~%~@5s " (quotient index line-width)))
      (cond ((> index total) 'nothing)
            ((= index offset)
             (let ((c (if (VIO/eof? vio)
                          (string-bold "$")
                          (char->string (vector-ref cont index)))))
               (display (string-red c))
               (iter (+ 1 index))))
            ((< index slen)
             (display (char->string (vector-ref cont index)))
             (iter (+ 1 index)))
            ((= index slen)
             (display (string-bold (string-yellow "$")))
             (iter (+ 1 index)))
            (else
             (display (string-blue (string-bold "X")))
             (iter (+ 1 index)))))
    (newline)))

(define (VIO/get-attr vio attr)
  (cond ((assq attr (cdr vio)) => cdr)
        (else '())))

(define (VIO/set-attr! vio attr val)
  (cond ((assq attr (cdr vio)) =>
         (lambda (entry)
           (set-cdr! entry val)))
        (else '())))

(define (VIO/tell vio)
  (VIO/get-attr vio 'offset))

(define (VIO/seek! vio val #!optional from-where)
  (let* ((vlength (VIO/get-length vio))
         (offset
          (cond ((default-object? from-where) val)
                ((eq? from-where 'begin)      val)
                ((eq? from-where 'current)    (+ (VIO/tell vio) val))
                ((eq? from-where 'end)        (+ vlength val))
                (else (error "wrong argument -- " from-where)))))
    (cond ((> offset vlength) (error "offset is great than length -- " offset))
          ((< offset 0)       (error "offset is less than length -- " offset))
          (else               (VIO/set-attr! vio 'offset offset)))))

(define (VIO/eof? vio)
  (equal? (VIO/tell vio) (VIO/get-length vio)))

(define (VIO/get-length vio)
  (VIO/get-attr vio 'length))

(define (VIO/get-content vio)
  (VIO/get-attr vio 'content))

(define (VIO/get-byte! vio)
  (if (VIO/eof? vio)
      (error "eof reached!")
      (let ((byte (vector-ref (VIO/get-content vio) (VIO/tell vio))))
        (VIO/seek! vio 1 'current)
        byte)))

(define (VIO/push-back! vio how)
  (VIO/seek! vio (- how) 'current))

;(define s "Venenatis felis do. Aute hendrerit tortor est. Per tincidunt cras augue eros. Erat nullam aliqua diam fermentum adipiscing. Ante montes posuere rutrum dignissim laborum.Fringilla laborum facilisis nullam nunclorem. Nostrud nullam a. Nascetur diam cum habitant dui commodo. Et non libero mattis aliquam. Sociis litora non conubiaFacilisi non morbi consectetur justo. Pretium porttitor esse praesent ridiculus. Mattis sodales vivamus ultricies ultrices. Venenatis et sollicitudin.Sunt laoreet dapibus cras dictum. Sit eu parturient. Occaecat augue felis tristique iaculis nostra.Deserunt cursus libero tristique cillum dolor. Ultricies nullam ornare cillum suspendisse dictum. Anim netus diam erat.")
;(define n "6a8ff0a00de05a9514dc47e38466241e11e1c94053608f2b3b4f0501aeea")
;(define v (make-vio s))
