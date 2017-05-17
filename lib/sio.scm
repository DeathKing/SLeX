;;; SIO.scm -- a vector implementation of IO stream.
(load-option 'format)
(load-relative "color.scm")

(define (make-sio #!optional cont)
  (let* ((cont (if (default-object? cont) "" cont)))
    (cons 'sio
          (list
            (cons 'offset 0)
            ;(cons 'length (string-length cont))
            (cons 'content cont)))))

(define (SIO/fill-in! sio str)
  (SIO/set-attr! sio 'cont (append (SIO/get-content sio) str)))

(define (SIO/inspect sio #!optional line-width)
  (let ((line-width (or (and (default-object? line-width) 60) line-width))
        (cont (SIO/get-content sio))
        (offset (SIO/tell sio))
        (slen (SIO/get-length sio))
        (mark-pos -1))
    (format #t "** SIO/inspect **~%")
    (format #t "--------------------------------------------------~%")
    (format #t " offset: ~s~%" offset)
    (format #t " length: ~s~%" slen)
    (format #t "--------------------------------------------------")
    (let iter ((index 0))
      (if (equal? 0 (modulo index line-width))
          (format #t "~%~@5s " (quotient index line-width)))
      (cond ((= index slen)
             (display (string-bold (string-yellow "$"))))
            ((= index offset)
             (let ((c (if (SIO/eof? sio)
                          (string-bold "$")
                          (char->string (string-ref cont index)))))
               (display (string-red c))
               (iter (+ 1 index))))
            (else
             (display (char->string (string-ref cont index)))
             (iter (+ 1 index)))))
    (newline)))

(define (SIO/get-attr sio attr)
  (cond ((assq attr (cdr sio)) => cdr)
        (else '())))

(define (SIO/set-attr! sio attr val)
  (cond ((assq attr (cdr sio)) =>
         (lambda (entry)
           (set-cdr! entry val)))
        (else '())))

(define (SIO/tell sio)
  (SIO/get-attr sio 'offset))

(define (SIO/seek! sio val #!optional from-where)
  (let* ((slen (SIO/get-length sio))
         (offset
          (cond ((default-object? from-where) val)
                ((eq? from-where 'begin)      val)
                ((eq? from-where 'current)    (+ (SIO/tell sio) val))
                ((eq? from-where 'end)        (+ slen val))
                (else (error "wrong argument -- " from-where)))))
    (cond ((> offset slen) (error "offset is great than length -- " offset))
          ((< offset 0)    (error "offset is less than length -- " offset))
          (else            (SIO/set-attr! sio 'offset offset)))))

(define (SIO/eof? sio)
  (equal? (SIO/tell sio) (SIO/get-length sio)))

(define (SIO/get-length sio)
  (string-length (SIO/get-content sio)))

(define (SIO/get-content sio)
  (SIO/get-attr sio 'content))

(define (SIO/get-byte! sio)
  (if (SIO/eof? sio)
      (error "eof reached!")
      (let ((byte (string-ref (SIO/get-content sio) (SIO/tell sio))))
        (SIO/seek! sio 1 'current)
        byte)))

(define (SIO/peek-byte sio)
  (if (SIO/eof? sio)
      (error "eof reached!")
      (string-ref (SIO/get-content sio) (SIO/tell sio))))

(define (SIO/push-back! sio count)
  (SIO/seek! sio (- count) 'current))

(define (SIO/subcontent sio start end)
  (substring (SIO/get-content sio) start end))

(define (SIO/offset sio where)
  (- (SIO/tell sio) where))

(define s "Venenatis felis do. Aute hendrerit tortor est. Per tincidunt cras augue eros. Erat nullam aliqua diam fermentum adipiscing. Ante montes posuere rutrum dignissim laborum.Fringilla laborum facilisis nullam nunclorem. Nostrud nullam a. Nascetur diam cum habitant dui commodo. Et non libero mattis aliquam. Sociis litora non conubiaFacilisi non morbi consectetur justo. Pretium porttitor esse praesent ridiculus. Mattis sodales vivamus ultricies ultrices. Venenatis et sollicitudin.Sunt laoreet dapibus cras dictum. Sit eu parturient. Occaecat augue felis tristique iaculis nostra.Deserunt cursus libero tristique cillum dolor. Ultricies nullam ornare cillum suspendisse dictum. Anim netus diam erat.")
(define n "6a8ff0a00de05a9514dc47e38466241e11e1c94053608f2b3b4f0501aeea")
(define v (make-sio s))
