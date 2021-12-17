#lang racket
; Ysabella Atehortua and Eamon McKeon

; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt.

; PART ONE

(provide (all-defined-out))

; atom

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

; list-of-atom

(define (list-of-atom? lst)
  (cond [(empty? lst) #t]
        [(atom? (first lst)) (list-of-atom? (rest lst))]
        [else #f]))

; not-list-of-atom

(define (not-list-of-atom? lst)
  (cond [(empty? lst) #f]
        [(atom? (first lst)) (not-list-of-atom? (rest lst))]
        [else #t]))

; list-of-int

(define (list-of-int? lst)
  (cond [(empty? lst) #t]
        [(integer? (first lst)) (list-of-int? (rest lst))]
        [else #f]))

; list-of-same

(define (list-of-same? pred lst)
  (cond [(empty? lst) #t]
        [(not(list? lst)) #f]
        [(pred (car lst))
         (list-of-same? pred (cdr lst))]
        [else #f]))

; make-list-of-same

(define (make-list-of-same pred)
  (λ (lst)
    (list-of-same? pred lst)))

; PART TWO

; all-members

(define (is-member x lst)
  (cond [(empty? lst) #f]
        [(equal? x (car lst)) #t]
        [else (is-member x (rest lst))]))

(define (all-members lst1 lst2)
  (cond [(empty? lst1) #t]
        [(is-member (car lst1) lst2) (all-members (rest lst1) lst2)]
        [else #f]))

; remove-second

(define (remove-second x lst)
  (cond
    ((null? lst) '())
    ((null? (cdr lst)) lst)
    (else (if (equal? x (car lst))
              (cons (car lst) (remove x (cdr lst)))
              (cons (car lst) (remove-second x (cdr lst)))))))

; remove-pair
(define (remove-next? x lst)
  (cond [(empty? lst) #f]
        [(equal? x (car lst))#t]
        [else #f]))

(define (remove-pair x lst)
  (cond [(empty? lst) empty]
        [(and (equal? x (car lst)) (remove-next? x (cdr lst))) (remove-pair x (cdr (cdr lst)))]
        [else (cons (car lst) (remove-pair x (cdr lst)))]))

; duplicate

(define (duplicate x lst)
  (cond [(zero? x) empty]
        [else (cons lst (duplicate (sub1 x) lst))]))

; maximum

(define (maximum lst)
  (cond
    ((null? lst) #f)
    ((null? (cdr lst))
     (car lst))
    ((< (car lst) (cadr lst))
     (maximum (cdr lst)))
    (else 
     (maximum (cons (car lst) (cddr lst))))))

; index-of

(define (check-count count x lst)
  (cond [(empty? lst) #f]
        [(equal? (car lst) x) count]
        [else (check-count (+ count 1) x (cdr lst))]))

(define (index-of lst x)
  (check-count 0 x lst))