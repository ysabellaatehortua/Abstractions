#lang racket

; ysabella atehortua and eamon mckeon

(require rackunit) (require rackunit rackunit/text-ui rackunit/gui)
(require "env.rkt" "parse.rkt" "interp.rkt")

(provide interp-tests)

(define test-env
  (env '(x y) '(10 23) init-env))

(define interp-tests
  (test-suite
   "Interpreter tests"
   (test-eqv? "Number"
             (eval-exp (lit-exp 5) empty-env)
             5)
   (test-eqv? "Symbol to value"
              (eval-exp '(var-exp x) test-env)
              10)
   (test-equal? "Prim-proc +"
                (eval-exp '(app-exp (var-exp +) ((lit-exp 3) (lit-exp 4)))
                          init-env)
                7)
   (test-equal? "Prim-proc *"
                (eval-exp '(app-exp (var-exp *) ((lit-exp 3) (lit-exp 4)))
                          init-env)
                12)
   (test-equal? "Prim-proc car on list"
                (eval-exp '(app-exp (var-exp car)
                                    ((app-exp (var-exp list)
                                              ((lit-exp 1) (lit-exp 2) (lit-exp 3)))))
                          init-env)
                1)
   (test-equal? "ite"
                (eval-exp '(ite-exp (var-exp x)
                                    (lit-exp 3)
                                    (lit-exp 4))
                          init-env)
                3)
   (test-equal? "Let"
                (eval-exp '(let-exp (a b)
                                    ((lit-exp 3)
                                     (lit-exp 9))
                                    (app-exp (var-exp +)
                                             ((var-exp a)
                                              (var-exp b))))
                          init-env)
                12)
   ))

(run-tests interp-tests)