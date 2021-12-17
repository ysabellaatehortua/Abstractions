#lang racket

; ysabella atehortua and eamon mckeon

(require rackunit)(require rackunit rackunit/text-ui rackunit/gui)
(require "parse.rkt")

(provide parse-tests)

(define parse-tests
  (test-suite
   "Parse tests"
   (test-pred "Literal"
           lit-exp?
           (parse 5))
   (test-equal? "Number extraction"
                (lit-exp-num (parse 7))
                7)
   (test-equal? "Symbol"
                (var-exp-symbol (parse 'x))
                'x)
   (test-equal? "Prim-proc +"
                (parse '(+ 3 4))
                '(app-exp (var-exp +) ((lit-exp 3) (lit-exp 4))))
   (test-equal? "Prim-proc *"
                (parse '(* 3 4))
                '(app-exp (var-exp *) ((lit-exp 3) (lit-exp 4))))
   (test-equal? "Prim-proc car on list"
                (parse '(car (list 1 2 3)))
                '(app-exp (var-exp car)
                                    ((app-exp (var-exp list)
                                              ((lit-exp 1) (lit-exp 2) (lit-exp 3))))))
   (test-equal? "If Then Else"
                (parse '(if x 3 4))
                '(ite-exp (var-exp x) (lit-exp 3) (lit-exp 4)))
   (test-equal? "Let Expression"
                (parse '(let ([a 3]
                              [b 9])
                          (+ a b)))
                '(let-exp (a b)
                          ((lit-exp 3)
                           (lit-exp 9))
                          (app-exp (var-exp +)
                                   ((var-exp a)
                                    (var-exp b)))))
   (test-pred "Lambda exp?"
              lambda-exp?
              (parse '(lambda (x y) (+ x y))))
   (test-equal? "2 arg lambda exp"
                (parse '(lambda (x y) (+ x y)))
                '(lambda-exp (x y) (app-exp (var-exp +)
                                            ((var-exp x) (var-exp y)))))
   (test-equal? "lambda with let"
                (parse '(let ([sqr (lambda (x)
                                     (* x x))])
                          (sqr 64)))
                '(let-exp (sqr)
                          ((lambda-exp (x)
                                       (app-exp (var-exp *)
                                                ((var-exp x)
                                                (var-exp x)))))
                          (app-exp (var-exp sqr) ((lit-exp 64)))))
   (test-equal? "set!"
                (parse '(set! x 10))
                '(set-exp x (lit-exp 10)))
   (test-equal? "set! with let and begin"
               (parse '(let ([a 1]
                             [b 2])
                         (begin
                           (set! a 3)
                           (set! b 4)
                           (+ a b))))
               '(let-exp (a b)
                         ((lit-exp 1) (lit-exp 2))
                         (begin-exp
                           ((set-exp a (lit-exp 3))
                           (set-exp b (lit-exp 4))
                           (app-exp (var-exp +) ((var-exp a) (var-exp b)))))))
   ))


(run-tests parse-tests)
