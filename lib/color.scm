; COLOR.scm -- make string colorful
(load-option 'format)

(define *c-bold*      "\033[1m")
(define *c-underline* "\033[4m")
(define *c-finalize*  "\033[0m")
(define *c-red*       "\033[91m")
(define *c-green*     "\033[92m")
(define *c-yellow*    "\033[93m")
(define *c-blue*      "\033[94m")
(define *c-cyan*      "\033[96m")

(define (string-bold str)
  (string-append *c-bold* str *c-finalize*))

(define (string-underline str)
  (string-append *c-underline* str *c-finalize*))

(define (string-red str)
  (string-append *c-red* str *c-finalize*))

(define (string-blue str)
  (string-append *c-blue* str *c-finalize*))

(define (string-yellow str)
  (string-append *c-yellow* str *c-finalize*))