#lang racket

; Eamon McKeon and Ysa Atehortua

; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt.

(provide (all-defined-out))

; PART 1

; firsts and rests

(define (firsts lsts)
  (map first lsts))

(define (rests lsts)
  (map rest lsts))

; vec-+

(define (vec-+ vec1 vec2)
  (map + vec1 vec2))

; dot-product

(define (dot-product vec1 vec2)
  (apply + (map * vec1 vec2)))

; mat-vec-*

(define (mat-vec-* mat vec)
  (cond [(empty? mat) empty]
        [else (cons (dot-product (first mat) vec) (mat-vec-* (rest mat) vec))]))

; transpose

(define (transpose mat)
  (cond [(empty? (first mat)) empty]
        [else (cons (firsts mat) (transpose (rests mat)))]))

; mat-mat-*

(define helper
  (λ (A B)
    (transpose (map (λ (x) (mat-vec-* A x))
                    (transpose B)))))

(define (mat-mat-* lhs rhs)
   (map
   (λ (row)
     (apply map
       (λ column      
         (apply + (map * row column)))
       rhs))
   lhs))

   
; PART 2

; flatten

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

; sum

(define (sum lst)
  (cond [(number? lst) lst]
        [else (apply + (map sum lst))]))

; map-to

 (define (map-to f lst)
  (let deep ((x lst))
    (cond ((null? x) x)
          ((pair? x) (map deep x))
          (else (f x)))))


; element-of

(define (element-of? a lst)
(cond [(empty? lst) #f]
          [(equal? (first lst) a) #t]
          [(list? (first lst)) (or (element-of? a (first lst)) (element-of? a (rest lst)))]
          [else (element-of? a (rest lst))]))
