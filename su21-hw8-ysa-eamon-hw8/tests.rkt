#lang racket
; Your name(s) here

(require rackunit rackunit/text-ui rackunit/gui)
(require "hw8.rkt")

(define test-stream
  (stream-cons 'x (stream-cons 'y (stream-cons 'z test-stream))))


(define stream-remove-all-tests
  (test-suite
   "Stream-remove-all"
   (test-equal? "Remove x's in test-stream"
                (stream->list(stream-take
                              (stream-remove-all 'x test-stream) 6))
                '(y z y z y z))
   (test-equal? "Remove element not present in test-stream"
                (stream->list(stream-take
                              (stream-remove-all 'a test-stream) 6))
                '(x y z x y z))
   (test-equal? "Remove z's in test-stream"
                (stream->list(stream-take
                              (stream-remove-all 'z test-stream) 10))
                '(x y x y x y x y x y))
   ))

(define stream-replace-tests
  (test-suite
   "Stream-replace"
   (test-equal? "Replace x's with a's"
                (stream->list(stream-take
                              (stream-replace 'x 'a test-stream) 8))
                '(a y z a y z a y))
   (test-equal? "Replaced element doesn't exist in stream"
                (stream->list(stream-take
                              (stream-replace 'o 'a test-stream) 8))
                '(x y z x y z x y))
   (test-equal? "Replace x's with null"
                (stream->list(stream-take
                              (stream-replace 'x null test-stream) 8))
                '(() y z () y z () y))
   ))

(define next-pair-tests
  (test-suite
   "Next-pair"
   (test-equal? "Very first pair"
                (next-pair (cons 1 1))
                (cons 1 2))
   (test-equal? "Middle of grid"
                (next-pair (cons 2 7))
                (cons 3 6))
   (test-equal? "Last of one diagonal"
                (next-pair (cons 7 1))
                (cons 1 8))
   ))


(define ham-tests
  (test-suite
   "Hamming sequence"
   ; You may wish to uncomment this to test once you have implemented ham.
    (test-equal? "1000th element of ham"
                 (stream-ref ham 1000)
                 51840000)
    (test-equal? "Assignment example, first 40 elements of ham"
                 (stream->list (stream-take ham 40))
                 '(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40 45 48 50
                     54 60 64 72 75 80 81 90 96 100 108 120 125 128 135 144))
   ))

(define powers-tests
  (test-suite
   "powers"
   (test-equal? "check if powers works"
                (stream->list (stream-take (powers 2) 5))
                '(1 2 4 8 16))
   (test-equal? "e-approx example"
                (stream->list (stream-take (e-approx 1) 5))
                '(1.0 2.0 2.5 2.6666666666666665 2.708333333333333))
   (test-equal? "sin-approx"
                (stream->list (stream-take (sin-approx 2) 5))
                '(0 2 2 -6 -6))
   (test-equal? "cos-approx"
                (stream->list (stream-take (cos-approx 2) 5))
                '(1 1 -3 -3 13))
   ))

(define grune-tests
  (test-suite
   "Grune"
   (test-equal? "grune-a-b test 1"
                (stream->list (grune-a-b '(a b c d a a a b a a)))
                '(a b c d b a b b))
   (test-equal? "grune-a-b test 2"
                (stream->list (grune-a-b '(a a a)))
                '(b a))
                ))

; Define an All Tests test suite.
(define all-tests
  (test-suite
   "All tests"
   stream-remove-all-tests
   stream-replace-tests
   next-pair-tests
   ham-tests
   grune-tests
   powers-tests))

(run-tests all-tests)
