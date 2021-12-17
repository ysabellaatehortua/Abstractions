#lang racket
; Ysa Atehortua and Eamon McKeon

(require racket/stream)
(require "keyboard.rkt")

(provide (all-defined-out))

(define test-stream
  (stream-cons 'x (stream-cons 'y (stream-cons 'z test-stream))))


; PART 1

(require racket/stream)

(define (stream-add s t)
  (cond [(stream-empty? s) empty-stream]
        [(stream-empty? t) empty-stream]
        [else (stream-cons (+ (stream-first s) (stream-first t))
                           (stream-add (stream-rest s) (stream-rest t)))]))

(define (integers-from n)
  (stream-cons n (integers-from (add1 n))))
(define ints (integers-from 0))
(define evens (stream-map (λ (n) (* 2 n)) ints))
(define ones (stream-cons 1 ones))
(define odds (stream-add ones evens))

(stream->list (stream-take ones 10))
(stream->list (stream-take odds 20))

; stream-remove-all

(define (stream-remove-all x s)
  (cond [(equal? (stream-first s) x) (stream-remove-all x (stream-rest s))]
        [else (stream-cons (stream-first s) (stream-remove-all x (stream-rest s)))]))


; stream-replace

(define (stream-replace x y s)
  (cond [(equal? (stream-first s) x) (stream-cons y (stream-replace x y (stream-rest s)))]
        [else (stream-cons (stream-first s) (stream-replace x y (stream-rest s)))]))


; PART 2

(define (pairs-from p)
  (stream-cons p (pairs-from (next-pair p))))

(define all-pairs (pairs-from (cons 1 1)))


; next-pair

(define (next-pair p)
  (if (equal? 1 (cdr p)) (cons 1 (+ (car p) 1))
      (cons (+ (car p) 1) (- (cdr p) 1))))
        

; stream-merge

(define (stream-merge s1 s2)
  (cond [(stream-empty? s1) s2]
        [(stream-empty? s2) s1]
        [(< (stream-first s1) (stream-first s2))
         (stream-cons (stream-first s1)
                      (stream-merge (stream-rest s1) s2))]
        [(> (stream-first s1) (stream-first s2))
         (stream-cons (stream-first s2)
                      (stream-merge s1 (stream-rest s2)))]
        [else (stream-merge (stream-rest s1) s2)]))


; ham

(define ham
  (stream-cons 1
                (stream-merge (stream-map (λ (n) (* 2 n)) ham)
                              (stream-merge (stream-map (λ (n) (* 3 n)) ham)
                                            (stream-map (λ (n) (* 5 n)) ham)))))


; PART 3

(define (partial-sums s)
  (cond [(stream-empty? s) empty-stream]
        [else
         (letrec ([sums (stream-add s (stream-cons 0 sums))])
           sums)]))

(define (stream-mul s t)
  (cond [(stream-empty? s) empty-stream]
        [(stream-empty? t) empty-stream]
        [else
         (stream-cons (* (stream-first s)
                         (stream-first t))
                      (stream-mul (stream-rest s)
                                  (stream-rest t)))]))

(define fact-stream
  (stream-cons 1 (stream-mul fact-stream (integers-from 1))))

(define e-coeffs
  (stream-map (λ (n) (/ 1.0 n)) fact-stream))

(define (e-approx x)
  (partial-sums (stream-mul (powers x) e-coeffs)))

(define sin-coeffs
  (stream-cons 0
               (stream-cons 1
                            (stream-cons 0
                                         (stream-cons -1 sin-coeffs)))))
(define cos-coeffs
  (stream-cons 1
               (stream-cons 0
                            (stream-cons -1
                                         (stream-cons 0 cos-coeffs)))))
(define powers
  (lambda(x)
    (stream-cons 1 (stream-map (lambda (t)
                   (* x t)) (powers x)))))

(define sin-approx
  (lambda(x)
    (partial-sums (stream-mul (powers x) sin-coeffs))))

(define cos-approx
  (lambda (x)
    (partial-sums (stream-mul  (powers x) cos-coeffs))))


; PART 4
(define grune-a-b
  (lambda (s)
    (cond
      [(stream-empty? s) 'empty-stream]
      [(eq? 'a (stream-first s)) (let grune2 ([f (stream-first (keyboard-stream))])
                           (if (eq? 'a f) (stream-cons 'b (grune-a-b (stream-rest s))) (stream-cons 'a (stream-cons f (grune-a-b (stream-rest s))))))]
      [else (stream-cons (stream-first s) (grune-a-b (stream-rest s)))])))

(define grune
  (lambda (a b)
    (lambda (s)
      (cond
      [(stream-empty? s) 'empty-stream]
      [(eq? a (stream-first s)) (let ([f (stream-first (stream-rest s))])
                           (if (eq? a f) (stream-cons b ((grune a b) (stream-rest (stream-rest s)))) (stream-cons a (stream-cons f ((grune a b) (stream-rest (stream-rest s)))))))]
      [else (stream-cons (stream-first s) ((grune a b) (stream-rest s)))]))))