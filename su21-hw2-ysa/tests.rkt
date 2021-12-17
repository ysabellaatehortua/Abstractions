#lang racket

(require rackunit rackunit/text-ui rackunit/gui)
(require "hw2.rkt")


; Define tests for merge.
(define merge-tests
  (test-suite
   "merge"
   (test-equal? "Odds and evens"
                (merge '(1 3 5 7 9) '(2 4 6 8 10))
                '(1 2 3 4 5 6 7 8 9 10))
   
   (test-equal? "Two empty lists"
                (merge null null)
                null)
   
   (test-equal? "hw example"
                (merge '(1 4 5) '(2 3 4 6))
                '(1 2 3 4 4 5 6))
   (test-equal? "One single integer list"
                (merge '(4) '(1 3 4 6 9))
                '(1 3 4 4 6 9))
   ))

; Define tests for sort
(define sort-tests
  (test-suite
   "sort"
   (test-equal? "empty list"
                (sort null)
                null)
   
   (test-equal? "hw example"
                (sort '(5 1 8 3 7))
                '(1 3 5 7 8))
  
   (test-equal? "One single integer list"
                (sort '(4))
                '(4))
   ))

; Define tests for contains-sublist
(define contains-sublist-tests
  (test-suite
   "contains sublist"
   (test-true "empty list"
               (contains-sublist '() '()))
   
   (test-true "hw example"
              (contains-sublist '(2 3 4) '(1 2 3 4 5)))
  
   (test-false "One single integer list"
               (contains-sublist '(2 3 4) '(1 2 5 3 4)))
   ))

; Define tests for remove-sublist

(define remove-sublist-tests
  (test-suite
   "remove sublist"
   (test-equal? "hw example 1"
                (remove-sublist '(2 3 4) '(1 2 3 4 5))
                '(1 5))
   (test-equal? "hw example 2"
                (remove-sublist '(2 3 4) '(1 2 5 3 4))
                '(1 2 5 3 4))
   (test-equal? "no match"
                (remove-sublist '(2 3) '(1 4 5 6))
                '(1 4 5 6))
   ))



; Define tests for phone book

(define phone-book-tests
  (test-suite
   "phone book"
   (test-equal? "hw example 1"
                (phone-number 'nick phone-book)
                '(775-0912))
   (test-equal? "hw example 2"
                (phone-number 'carol phone-book)
                'disconnected)
   (test-equal? "abbr. name"
                (phone-number 'val phone-book)
                'disconnected)
   ))

; Define tests for person

(define person-tests
  (test-suite
   "person"
   (test-equal? "hw example 1"
                (person '775-0912 phone-book)
                'nick)
   (test-equal? "wrong format"
                (person 'carol phone-book)
                'disconnected)
   (test-equal? "abbr. number"
                (person '774 phone-book)
                'disconnected)
   ))

; Define tests for deepen
(define deepen-tests
  (test-suite
   "deepen"
   (test-equal? "empty list"
                (deepen null)
                null)
   (test-equal? "hw example 1"
                (deepen '(a b c))
                '((a) (b) (c)))
   (test-equal? "hw example 2"
                (deepen '(a (b (c d)) e))
                '((a) ((b (c d))) (e)))
   ))

; Define tests for eval-bin
(define eval-bin-tests
  (test-suite
   "eval-bin"
   (test-equal? "hw example 1"
                (eval-bin '(1 0 1 1) 0)
                11)
   (test-equal? "hw example 2"
                (eval-bin '(1 1 0) 0)
                6)
   (test-equal? "empty list"
                (eval-bin '() 0)
                0)
   ))

; Define tests for sub
(define sub-tests
  (test-suite
   "sub"
   (test-equal? "hw example 1"
                (sub 'a 'x '(a b r a c a d a b r a))
                '(x b r x c x d x b r x))
   (test-equal? "empty list"
                (sub 'b 'c '())
                '())
   (test-equal? "no match"
                (sub 'a 'x '(b c d e f))
                '(b c d e f))
   ))

; Define tests for subs
(define subs-tests
  (test-suite
   "subs"
   (test-equal? "hw example 1"
                (subs '(b) '(m) '(b o b))
                '(m o m))
   (test-equal? "hw example 2"             
                (subs '(b o) '(m u) '(b o b))
                '(m u m))
   (test-equal? "empty list"
                (subs '(b o) '(m u) null)
                null)
   ))

; Define tests for subs
(define replace-tests
  (test-suite
   "replace"
   (test-equal? "hw example 1"
                (replace '(b o) '(m u) 'a)
                'a)
   (test-equal? "hw example 2"             
                (replace '(b o) '(m u) 'b)
                'm)            
   ))

; Define an All Tests test suite.
(define all-tests
  (test-suite
   "All tests"
   merge-tests
   sort-tests
   contains-sublist-tests
   remove-sublist-tests
   deepen-tests
   phone-book-tests
   person-tests
   eval-bin-tests
   sub-tests
   subs-tests
   replace-tests
   ))

(run-tests all-tests)
