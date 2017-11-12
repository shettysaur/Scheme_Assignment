#lang racket

(define in (open-input-file "t0.in"))

(define l '())

(define x '())
(set! x (string-split(read-line in)))

(define n 0)
(set! n (string->number(car x)))
;(display n)

(define (masread l count)
  (cond
    ((= count (+ n 1)) l)
  (else (masread (append l (list (reverse (cons (string-split(read-line in)) (list count)) ))) (+ count 1)))
  ))
(display (masread '() 1))
