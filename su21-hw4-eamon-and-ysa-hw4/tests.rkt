#lang racket
; Your name(s) here

(require rackunit rackunit/text-ui rackunit/gui)
(require "hw4.rkt")
(require "TreeDatatype.rkt")


; Define tests for index
(define index-tests
  (test-suite
   "index"
   (test-equal? "hw example 1"
                (index 3 '(2 4 3 1 2 3))
                2)
   (test-equal? "hw example 2"
                (index 3 '(2 4 6 4 2))
                -1)
   (test-equal? "trying characters"
                (index 'c '(a b c d e))
                2)
   ))

; Define tests for replace
(define replace-tests
  (test-suite
   "replace"
   (test-equal? "hw example 1"
                (replace 3 5 '(1 2 3 5 4 3 2 1))
                '(1 2 5 5 4 5 2 1))
   (test-equal? "hw example 2"
                (replace 3 5 '(2 4 6 8))
                '(2 4 6 8))
   (test-equal? "using characters"
                (replace 'a 'b '(b a b a b a b a))
                '(b b b b b b b b))
    ))

; Define tests for weigh
(define weigh-tests
  (test-suite
   "weigh"
   (test-equal? "hw example"
                (weigh bags)
                87)
   ))

; Define tests for heaviest
(define heaviest-tests
  (test-suite
   "heaviest"
   (test-equal? "hw example"
                (heaviest bags)
                'steamer-trunk)
   ))

; Define tests for child-sum
(define child-sum-tests
  (test-suite
   "child-sum"
   (test-equal? "hw example 1"
                (child-sum T8)
                173)
   (test-equal? "hw example 2"
                (child-sum T6)
                82)
   (test-equal? "hw example 1"
                (child-sum empty-tree)
                0)
   ))

;Define tests for all-sum
(define all-sum-tests
  (test-suite
   "all-sum"
   (test-equal? "hw example 1"
                (all-sum T8)
                293)
   (test-equal? "hw example 2"
                (all-sum T6)
                155)
   (test-equal? "hw example 3"
                (all-sum empty-tree)
                0)
   ))

;Define tests for visit-tree
(define visit-tree-tests
  (test-suite
   "visit-tree"
   (test-equal? "test T1"
                (visit-tree add1 T1)
                '(tree 51 ()))
   (test-equal? "test T3"
                (visit-tree add1 T4)
                '(tree 6()))
   (test-equal? "empty tree"
                (visit-tree add1 empty-tree)
                '())
   ))

; Define tests for sizeof
(define sizeof-tests
  (test-suite
   "sizeof"
   (test-equal? "hw example1"
                (sizeof T8)
                8)
   (test-equal? "hw example 2"
                (sizeof T6)
                4)
   (test-equal? "test tree1"
                (sizeof T1)
                1)
   ))

; Define tests for height
(define height-tests
  (test-suite
   "height"
   (test-equal? "hw example 1"
                (height empty-tree)
                -1)
   (test-equal? "hw example 2"
                (height T1)
                0)
   (test-equal? "hw example 3"
                (height T8)
                2)
   ))

; Define tests for preorder
(define preorder-tests
  (test-suite
   "preorder"
   (test-equal? "hw example 1"
                (preorder T8)
                '(16 73 50 22 10 100 5 17))
   (test-equal? "test T2"
                (preorder T2)
                '(22))
   (test-equal? "test T7"
                (preorder T4)
                '(5))
   ))

; Define tests for postorder
(define postoder-tests
  (test-suite
   "postorder"
   (test-equal? "hw example 1"
                (postorder T8)
                '(50 22 10 73 5 17 100 16))
   (test-equal? "test T4"
                (postorder T4)
                '(5))
   (test-equal? "test T1"
                (postorder T1)
                '(50))
   ))


; Define an All Tests test suite.
(define all-tests
  (test-suite
   "All tests"
   index-tests
   replace-tests
   weigh-tests
   heaviest-tests
   child-sum-tests
   all-sum-tests
   visit-tree-tests
   sizeof-tests
   height-tests
   preorder-tests
   postoder-tests
   ))

(run-tests all-tests)
