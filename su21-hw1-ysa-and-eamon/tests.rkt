#lang racket
; Ysabella Atehortua and Eamon McKeon

(require rackunit rackunit/text-ui rackunit/gui)
(require "hw1.rkt")

; Define tests for atom?.
(define atom?-tests
  (test-suite
   "atom?"
   (test-true "3 is an atom"
              (atom? 3))
   (test-false "Lists are not atoms"
               (atom? '(1 2)))
   (test-false "Pairs are not atoms"
               (atom? (cons 1 2)))
   (test-true "Booleans are atoms"
              (atom? #f))
   (test-false "null is not an atom"
               (atom? null))
   ))


; Define tests for list-of-atom?.
(define list-of-atom?-tests
  (test-suite
   "list-of-atoms?"
   (test-true "Empty list"
              (list-of-atom? empty))
   (test-true "Single element list returns true"
              (list-of-atom? '(1)))
   ; (test-false "lists with nulls are not atoms"
   ;            (list-of-atom? '(1 2 null 3)))
   ))

; Define tests for not-list-of-atom?.
(define not-list-of-atom?-tests
  (test-suite
   "not-list-of-atoms?"
   (test-false "Empty list"
               (not-list-of-atom? empty))
   (test-false "Single element list returns false"
               (not-list-of-atom? '(1)))
   ;  (test-true "lists with nulls are not atoms"
   ;  (not-list-of-atom? '(1 2 null 3)))
   ))

; Define tests for list-of-int?.
(define list-of-int?-tests
  (test-suite
   "list-of-int?"
   (test-true "Empty list"
              (list-of-int? empty))
   (test-true "Single element list returns true"
              (list-of-int? '(1)))
   (test-false "Lists of chars are not ints"
               (list-of-int? '(a b c)))
   ))

; Define tests for list-of-same?.
(define list-of-same?-tests
  (test-suite
   "list-of-same?"
   (test-true "empty list"
              (list-of-same? atom? '()))
   (test-false "Single element list returns false"
               (list-of-same? list?  '(1)))
   (test-true "list of integers returns true"
              (list-of-same? integer? '(4 3 1)))
   ))

; Define tests for make-list-of-same.
(define make-list-of-same-tests
  (test-suite
   "make-list-of-same"
   (test-false "integer and list returns false"
               ((make-list-of-same integer?) '(25 foo 18)))
   (test-false "Single element list returns false"
               ((make-list-of-same list? )'(1)))
   (test-true "4 is an integer"
              ((make-list-of-same integer?) '(4 3 1)))
   ))

; Define tests for all-members.
(define all-members-tests
  (test-suite
   "all-members"
   (test-true "acx is in list 2"
              (all-members '(a c x) '(a b x c x d)))
   (test-false "if list 2 is empty, return false"
               (all-members '(a) empty))
   (test-true "if both lists are empty, return true"
              (all-members empty empty))
   ))

; Define tests for remove-second.
(define remove-second-tests
  (test-suite
   "remove-second"
   (test-equal? "empty list"
                (remove-second 'x empty)
                empty)
   (test-equal? "Singleton, matching"
                (remove-second 'x '(x))
                '(x))
   (test-equal? "3 xs in a list"
                (remove-second 'x '(a b x c x d x e))
                '(a b x c d x e))
   ))

; Define tests for duplicate
(define duplicate-tests
  (test-suite
   "duplicate"
   (test-equal? "3 xs"
                (duplicate 3 'x)
                '(x x x))
   (test-equal? "no copies"
                (duplicate 0 'y)
                '())
   (test-equal? "copy list"
                (duplicate 3 '(a b c))
                '((a b c) (a b c) (a b c)))
   ))

; Define tests for maximum
(define maximum-tests
  (test-suite
   "maximum"
   (test-equal? "returns max?"
                (maximum '(4 6 3 4 5 1 2))
                6)
   (test-equal? "list of same number"
                (maximum '(3 3 3 3))
                3)
   (test-false "empty list"
                (maximum '())
   )))

; Define tests for index-of.
(define index-of-tests
  (test-suite
   "index-of"
   (test-equal? "return 0"
                (index-of '(x y z z y) 'x)
                0)
   (test-equal? "return 1"
                (index-of '(x y z z y) 'y)
                1)
   (test-false "empty"
               (index-of empty 'x))
   ))

; Define tests for remove-pair
(define remove-pair-tests
  (test-suite
   "remove-pair"
   (test-equal? "remove correctly"
                (remove-pair 'a '(a a b b c c a b c a a))
                '(b b c c a b c))
   (test-equal? "test 2"
                (remove-pair 'a '(a b c a b c a))
                '(a b c a b c a))
   (test-equal? "test 3"
                (remove-pair 'b '(a b b b b a))
                '(a a))))

; Define an All Tests test suite.
(define all-tests
  (test-suite
   "All tests"
   atom?-tests
   list-of-atom?-tests
   not-list-of-atom?-tests
   list-of-int?-tests
   list-of-same?-tests
   make-list-of-same-tests
   all-members-tests
   remove-second-tests
   remove-pair
   duplicate-tests
   maximum-tests
   index-of-tests
   remove-pair-tests
   ))

(run-tests all-tests)
