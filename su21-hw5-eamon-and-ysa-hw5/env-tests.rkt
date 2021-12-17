#lang racket

(require rackunit rackunit/text-ui rackunit/gui)
(require "env.rkt")

; Define an environment for testing.
(define test-env
  (env '(x y) '(1 2) empty-env))
(define a-test-env
  (env '(a b) '(3 4) test-env))
(define b-test-env
  (env '(c d) '(5 6) a-test-env))
(define c-test-env
  (env '(d e) '(0 10) b-test-env))

(define env-tests
  (test-suite
   "Environment tests"
   (test-true "Empty environment recognizer"
              (empty-env? empty-env))
   
   (test-true "Empty environment is an environment"
              (env? empty-env))
   
   (test-false "Empty environment is not extended"
               (extended-env? empty-env))

   (test-true "Extended environment recognizer"
              (extended-env? test-env))

   (test-true "Extended environment is an environment"
              (env? test-env))

   (test-false "Extended environment is not empty"
               (empty-env? test-env))

   (test-equal? "Symbols accessor"
                (env-syms test-env)
                '(x y))

   (test-equal? "Values accessor"
                (env-vals test-env)
                '(1 2))

   (test-equal? "Previous environment accessor"
                (env-previous test-env)
                empty-env)

   (test-equal? "Previous environment accessor with non-empty previous"
                (env-previous (env '(z) '(3) test-env))
                test-env)

   (test-exn "Empty environment has no previous"
             exn:fail?
             (λ () (env-previous empty-env)))

   (test-exn "Symbol not bound in env"
             exn:fail?
             (λ () (env-lookup test-env 'z)))

   (test-exn "Symbol not bound in top-most env or previous ones"
             exn:fail?
             (λ () (env-lookup b-test-env 'g)))

   (test-exn "Empty env symbol lookup"
             exn:fail?
             (λ () (env-lookup empty-env 'x)))

   (test-equal? "Symbol lookup in environment"
                (env-lookup test-env 'y)
                2)

   (test-equal? "Symbol bound in a prev environment"
                (env-lookup b-test-env 'b)
                4)

   (test-equal? "Symbol bound in both an env and its previous one"
                (env-lookup c-test-env 'd)
                0)))

(run-tests env-tests)
