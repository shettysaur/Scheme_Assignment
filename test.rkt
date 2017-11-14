#lang racket

(define in (open-input-file "t1.in"))

(define l '())
(define ls '())

(define x '())
(set! x (string-split(read-line in)))

(define n 0)
(set! n (string->number(car x)))

(define d 0)
(set! d (string->number(car(cdr x))))

(define k 0)
(set! k (string->number(car(cdr(cdr x)))))

(define e 0)
(set! e (string->number(car(cdr(cdr(cdr x))))))

(define minpts 0)
(set! minpts (string->number(car(cdr(cdr(cdr(cdr x)))))))

;(display d)

(define (step1 l count)
  (cond
    ((= count (+ n 1)) l)
  (else (step1 (append l (list (reverse (cons (string-split(read-line in)) (list count)) ))) (+ count 1)))
  ))
(set! ls (step1 '() 1))
;(display ls)
(define (dist l1 l2 sum)
  (cond
    ((null? l1) (sqrt sum))
  (else (dist (cdr l1) (cdr l2) (+ sum (expt (- (string->number(car l1)) (string->number(car l2))) 2))))
  )
  )
;(display(car (cdr (car (cdr ls)))))
;(display (dist (car (cdr (car ls))) (car (cdr (car ls))) 0))

(define ans2 '())
(define ans3 '())
(define ans4 '())
(define ans5 '())
(define ans6 '())

(define (step2 ls1 ls2 ls3 lsr lsf)
  (cond
    ((null? ls1) lsf)
    ((null? ls2) (step2 (cdr ls1) ls3 ls3 '() (append lsf (list lsr))))
    ((= (car (car ls1)) (car (car ls2))) (step2 ls1 (cdr ls2) ls3 (append lsr (list (list (car (car ls2)) +inf.0))) lsf))
    (else (step2 ls1 (cdr ls2) ls3 (append lsr (list (list (car (car ls2)) (dist (car (cdr (car ls1))) (car (cdr (car ls2))) 0)))) lsf))
   )
  )
(set! ans2 (step2 ls ls ls '() '()))

(define (sort-asc-by-second lst)
  (sort lst
        (lambda (x y) (< (car (cdr x)) (car (cdr y))))))

(define (sort-desc-by-second lst)
  (sort lst
        (lambda (x y) (> (car (cdr x)) (car (cdr y))))))
;(sort-asc-by-second ans2)

(define (firstk l1 count lres)
  (cond
    ((= count k) (sort lres <))
    (else (firstk (cdr l1) (+ count 1) (append lres (list(car (car l1))))))
    )
  )
;(firstk (sort-asc-by-second (car ans2)) 0 '())

(define (step3 ls1 lres)
  (cond
    ((null? ls1) lres)
    (else (step3 (cdr ls1) (append lres (list (firstk (sort-asc-by-second (car ls1)) 0 '())))))
    )
  )
(set! ans3 (step3 ans2 '()))
;(display ans3)

(define (common ls1 ls2 count)
  (cond
    ((or (null? ls1) (null? ls2)) count)
    ((= (car ls1) (car ls2)) (common (cdr ls1) (cdr ls2) (+ count 1)))
    ((< (car ls1) (car ls2)) (common (cdr ls1) ls2 count))
    (else (common ls1 (cdr ls2) count))
    )
  )
(common '(5 8 12 13 14) '(1 5 12 13 14) 0)

(define (nth ls n)
  (cond
    ((= n 1) (car ls))
    (else (nth (cdr ls) (- n 1)))
    )
  )

(define (func ls1 ls2 ls resl n)
  (cond
    ((null? ls1) (sort-desc-by-second resl))
    ((not (member n (nth ls (car ls1)))) (func (cdr ls1) ls2 ls resl n))
    (else (func (cdr ls1) ls2 ls (append resl (list (append (list (car ls1)) (list (common (nth ls (car ls1)) ls2 0))))) n)) 
    )
  )

(define (step4 ls1 ls2 count resl)
  (cond
    ((null? ls1) resl)
    (else (step4 (cdr ls1) ls2 (+ count 1) (append resl (list (func (car ls1) (car ls1) ls2 '() (+ count 1))))))
    )
  )
(set! ans4 (step4 ans3 ans3 0 '()))
(display ans4)

(define (func1 ls count)
  (cond
    ((null? ls) count)
    ((< (car (cdr (car ls))) e) (func1 (cdr ls) count))
    (else (func1 (cdr ls) (+ count 1)))
    )
  )

(define (step5 ls lres)
  (cond
    ((null? ls) lres)
    (else (step5 (cdr ls) (append lres (list (func1 (car ls) 0)))))
    )
  )

(set! ans5 (step5 ans4 '()))

(define (step6 ls lres count)
  (cond
    ((null? ls) lres)
    ((>= (car ls) minpts) (step6 (cdr ls) (append lres (list count)) (+ count 1)))
    (else (step6 (cdr ls) lres (+ count 1)))
    )
  )

(set! ans6 (step6 ans5 '() 1))

(define (kdist ls n1 lres)
  (cond
    ((null? ls) lres)
    ((not (member (car ls) (nth ans3 n1))) (kdist (cdr ls) n1 lres))
    ((>= (common (nth ans3 n1) ( nth ans3 (car ls)) 0) e) (kdist (cdr ls) n1 (append lres (list (car ls)))))
    (else (kdist (cdr ls) n1 lres))
    )
  )

(define (mod_append ls1 ls2 lres)
  (cond
    ((null? ls2) lres)
    ((member (car ls2) ls1) (mod_append ls1 (cdr ls2) lres))
    (else (mod_append (append ls1 (list (car ls2))) (cdr ls2) (append lres (list (car ls2)))))
    )
  )

(define (func2 ls1 ls2)
  (cond
    ((null? ls1) (sort ls2 <))
    (else (func2 (append (cdr ls1) (mod_append ls2 (kdist ans6 (car ls1) '() ) '())) (append ls2 (mod_append ls2 (kdist ans6 (car ls1) '()) '()))))
    )
  )

(define (del ls1 ls2)
  (cond
    ((null? ls2) ls1)
    (else (del (remove (car ls2) ls1) (cdr ls2)))
    )
  )

(define (step7 crpts count lres)
  (cond
    ((null? crpts) lres)
    (else (step7 (del crpts (func2 (list (car crpts)) (list (car crpts)))) (+ count 1) (append lres (list (append (list count) (list (func2 (list (car crpts)) (list (car crpts)))))))))
    )
  )