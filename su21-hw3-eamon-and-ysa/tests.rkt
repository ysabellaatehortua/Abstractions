#lang racket

; Eamon McKeon and Ysa Atehortua

(require rackunit rackunit/text-ui rackunit/gui)
(require "hw3.rkt")


; Define tests for firsts
(define firsts-tests
  (test-suite
   "firsts"
   (test-equal? "List of single element lists"
                (firsts '((a) (b) (c)))
                '(a b c))

   (test-equal? "List of multiple element lists"
                (firsts '((9 4 2) (1 0 8) (7 5 6)))
                '(9 1 7))))


; Define tests for rests
(define rests-tests
  (test-suite
   "rests"
   (test-equal? "List of single element lists"
                (rests '((a) (b) (c)))
                '(() () ()))

   (test-equal? "List of multiple element lists"
                (rests '((9 4 2) (1 0 8) (7 5 6)))
                '((4 2) (0 8) (5 6)))))

; Define tests for vec-+
(define vec-+-tests
  (test-suite
   "vec-+"
   (test-equal? "Single element vectors"
                (vec-+ '(2) '(3))
                '(5))

   (test-equal? "Multiple element vectors"
                (vec-+ '(1 2 3) '(4 5 6))
                '(5 7 9))
   
   (test-equal? "Empty vectors"
                (vec-+ empty empty)
                empty)))


; Define tests for dot-product
(define dot-product-tests
  (test-suite
   "dot-product"
   (test-equal? "Single element vectors"
                (dot-product '(3) '(4))
                12)

   (test-equal? "Multiple element vectors"
                (dot-product '(1 2 3) '(4 5 6))
                32)

   (test-equal? "Empty vectors"
                (dot-product empty empty)
                0)))


; Define tests for mat-vec-*
(define mat-vec-*-tests
  (test-suite
   "mat-vec-*"
   (test-equal? "Single element matrix and vector"
                (mat-vec-* '((1)) '(2))
                '(2))

   (test-equal? "Multiple element matrix and vector"
                (mat-vec-* '((1 4 7) (2 5 8) (3 6 9)) '(1 2 3))
                '(30 36 42))

   (test-equal? "Multiple element matrix and vector with a 0"
                (mat-vec-* '((2 3 4) (1 1 1)) '(1 0 1))
                '(6 2))

   (test-equal? "Empty matrix and vector"
                (mat-vec-* empty empty)
                empty)))


; Define tests for transpose
(define transpose-tests
  (test-suite
   "transpose"
   (test-equal? "Matrix of single elements into one list"
                (transpose '((1) (2) (3)))
                '((1 2 3)))

   (test-equal? "Matrix of of single 3-element list into 3 single element lists"
                (transpose '((1 2 3)))
                '((1) (2) (3)))

   (test-equal? "3-list matrix of 3-element lists"
                (transpose '((1 2 3) (4 5 6) (7 8 9)))
                '((1 4 7) (2 5 8) (3 6 9)))

   (test-equal? "3-list matrix of 2-element lists into 2-list matrix of 3-element lists"
                (transpose '((1 2 3) (4 5 6)))
                '((1 4) (2 5) (3 6)))

   (test-equal? "Matrix of empty lists"
                (transpose '(() () ()))
                '())))

; Define tests for mat-mat
(define mat-mat-*-tests
  (test-suite
   "mat-mat-*"
   (test-equal? "hw example 1"
                (mat-mat-* '((1 0 1) (2 1 1)) '((1 2) (1 0) (1 1)))
                '((2 3) (4 5)))
   (test-equal? "simple matrix"
                (mat-mat-* '((1 1 1) (1 1 1)) '((2 2) (2 2) (2 2)))
                '((6 6) (6 6)))
   ))

; Define tests for flatten
(define flatten-tests
  (test-suite
   "flatten"
   (test-equal? "hw example 1"
                (flatten '(x y z z y))
                '(x y z z y))
   (test-equal? "hw example 2"
                (flatten '(a (x (y)) (((y)) y z)))
                '(a x y y y z)
                )
   (test-equal? "simple list"
                (flatten '(1 (2) (3) (4)))
                '(1 2 3 4))
   ))
; Define tests for sum
(define sum-tests
  (test-suite
   "sum"
   (test-equal? "hw example 1"
                (sum '((1 (2)) (((4))) 5))
                12)
   (test-equal? "random"
                (sum '(2 2 5 6))
                15)
   (test-equal? "empty list"
                (sum '())
                0)
   ))

; Define tests for map-to
(define map-to-tests
  (test-suite
   "map-to"
   (test-equal? "hw example 1"
              (map-to add1 '(3 (4 5)))
              '(4 (5 6)))
   (test-equal? "empty list"
                (map-to add1 '())
                '())
   (test-equal? "random"
                (map-to add1 '(4 5 6 7 7))
                '(5 6 7 8 8))
   ))
            
; Define tests for element-of
(define element-of-tests
  (test-suite
   "element-of"
   (test-true "hw example 1"
    (element-of? 3 '(2 1 (4 2 (5 3) 1))))
   (test-false "hw example 2"
    (element-of? 'x '(a (b c (d)) e f)))
   ))
    

; Define an All Tests test suite.
(define all-tests
  (test-suite
   "All tests"
   firsts-tests
   rests-tests
   vec-+-tests
   dot-product-tests
   mat-vec-*-tests
   transpose-tests
   element-of-tests
   map-to-tests
   sum-tests
   flatten-tests
   mat-mat-*-tests))

(run-tests all-tests)
