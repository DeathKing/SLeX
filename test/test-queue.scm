(load-relative "../lib/queue.scm")

(define q (make-queue))

(queue-add-item! q 1)
(queue-add-item! q 2)
(queue-add-item! q 3)
(queue-add-item! q 4)