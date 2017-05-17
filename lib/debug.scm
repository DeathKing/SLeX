; DEBUG.scm -- useful infrastructure for debugging.

(define (nanobench desc exp)
  (format #t "** [Nanobench] **~%")
  (format #t "  * Expression: ~s~%" desc)
  (with-timings exp
    (lambda (run-time gc-time real-time)
      (let ((gc-time   gc-time)
            (cpu-time  run-time) 
            (real-time real-time))
        (format #t " ---------------------------------------------------------~%")
        (format #t "  *  run-time: ~A~%" run-time)
        (format #t "  *   gc-time: ~A~%" gc-time)
        (format #t "  * real-time: ~A~%" real-time)))))

(define-syntax @nanobench
  (syntax-rules ()
    ((_ exp)
     (nanobench (quote exp) (lambda () exp)))))