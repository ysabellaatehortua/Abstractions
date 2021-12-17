#lang racket
; Your name(s) here.

; Eamon McKeon and Ysabella Atehortua

; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt.

(provide (all-defined-out))


; subset-sum

(define (subset-sum n nums)
  (bt n (rest nums) empty (first nums)))

(define (feasible x)
  (if (number? x)
      #t
      #f))

(define (bt n nums sofar curr)
  (cond [(equal? n (apply + sofar)) (reverse sofar)]
        [(equal? n (+ curr (apply + sofar))) (reverse (cons curr sofar))]
        [(empty? nums) #f]
        [(feasible curr)
         (let ([res (bt n (rest nums) (cons curr sofar) (first nums))])
           (if res
               res
               (bt n (rest nums) sofar (first nums))))]
        [else (bt n (rest nums) sofar (first nums))]))


; no-repeat

(define (no-repeat n)
  (no-repeat-bt n empty (add1 (random 3))))

(define (feasible-2 sofar curr)
  (cond [(empty? sofar) #t]
        [(list-prefix? curr sofar) #f]
        [else (feasible-2 (rest sofar) (append curr (list (first sofar))))]))

(define (no-repeat-bt n sofar curr)
  (cond [(equal? (length sofar) n) (reverse sofar)]
        [(> curr 3) #f]
        [(feasible-2 sofar (list curr))
         (let ([res (no-repeat-bt n (cons curr sofar) 1)])
           (if res
               res
               (no-repeat-bt n sofar (add1 curr))))]
        [else (no-repeat-bt n sofar (add1 curr))]))

  