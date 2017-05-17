; COLOR.scm -- make string colorful
(load-option 'format)

; Reference:
;   + https://en.wikipedia.org/wiki/ANSI_escape_code
;
; Foreground Code  30     31    32     33    34       35   36     37
; Background Code  40     41    42     43    44       45   46     47
; Normal           Black Red Green Yellow  Blue  Magenta Cyan  White
; Bright           Black Red Green Yellow  Blue  Magenta Cyan  White

(define *c-bold*      "\033[1m")
(define *c-underline* "\033[4m")
(define *c-finalize*  "\033[0m")
(define *c-red*       "\033[31m")
(define *c-green*     "\033[32m")
(define *c-yellow*    "\033[33m")
(define *c-blue*      "\033[34m")
(define *c-magenta*   "\033[35m")
(define *c-cyan*      "\033[36m")
(define *c-white*     "\033[37m")

(define (string-bold str)
  (string-append *c-bold* str *c-finalize*))

(define (string-underline str)
  (string-append *c-underline* str *c-finalize*))

(define (string-red str)
  (string-append *c-red* str *c-finalize*))

(define (string-green str)
  (string-append *c-green* str *c-finalize*))

(define (string-yellow str)
  (string-append *c-yellow* str *c-finalize*))

(define (string-blue str)
  (string-append *c-blue* str *c-finalize*))

(define (string-magenta str)
  (string-append *c-magenta* str *c-finalize*))

(define (string-cyan str)
  (string-append *c-cyan* str *c-finalize*))

(define (string-white str)
  (string-append *c-white* str *c-finalize*))
