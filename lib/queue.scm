;;; SLeX -- queue.scm
(load-relative "util.scm")

(define (make-queue)
  (cons 'queue (cons '() '())))

(define (make-queue1 init)
  (let ((new-pair (cons init '())))
    (cons 'queue (cons new-pair new-pair))))

(define (list->queue lst)
  (let ((queue (make-queue)))
    (for-each (lambda (e) (queue-enqueue! queue e)) lst)
    queue))

(define (queue? object)
  (and (not (null? object))
       (pair? object)
       (eq? 'queue (car object))))

(define (empty-queue? queue)
  (type-check queue queue? 'empty-queue?)
  (null? (%front-ptr queue)))

(define (queue-length queue)
  (type-check queue queue? 'queue-length)
  (length (%front-ptr queue)))

;;; ** NOTICE **  procedure whose name is start with symbol `%` is tend to
;;;               used locally (in other word, not as open as a library one)
(define (%set-front-ptr! queue item)
  (set-car! (cdr queue) item))

(define (%set-rear-ptr! queue item)
  (set-cdr! (cdr queue) item))

(define (%front-ptr queue) (cadr queue))
(define (%rear-ptr queue) (cddr queue))

(define (queue-front-ptr queue)
  (type-check queue queue? 'queue-front-ptr)
  (%front-ptr queue))

(define (queue-rear-ptr queue)
  (type-check queue queue? 'queue-rear-ptr)
  (%rear-ptr queue))

;;; ** NOTICE **
;;;
;;;    The API of `queue-memq` is slightly different from library
;;;    procedure `memq`.
(define (queue-memq queue object)
  (type-check queue queue? 'queue-memq)
  (memq object (%front-ptr queue)))

(define (queue->list queue)
  (type-check queue queue? 'queue->list)
  (let iter ((item (%front-ptr queue)) (result '()))
    (if (null? item)
        (reverse result)
        (iter (cdr item) (cons (car item) result)))))

(define (queue-enqueue! queue item)
  (type-check queue queue? "queue-enqueue!")
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (%set-front-ptr! queue new-pair)
           (%set-rear-ptr! queue new-pair))
          (else
           (set-cdr! (%rear-ptr queue) new-pair)
           (%set-rear-ptr! queue new-pair)))
    queue))

(define (queue-dequeue! queue)
  (type-check queue queue? 'queue-dequeue!)
  (cond ((empty-queue? queue)
         (error "BoundOverflow queue is empty"))
        (else
         (let ((head (%front-ptr queue)))
           (%set-front-ptr! queue (cdr head))
           (car head)))))


