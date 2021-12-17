#lang racket

; ysabella atehortua and eamon mckeon

(require "parse.rkt")
(require "env.rkt")

(provide eval-exp
         init-env)



(define (eval-exp tree e)
  (cond [(lit-exp? tree) (lit-exp-num tree)]
        [(var-exp? tree)
         (unbox (env-lookup e (var-exp-symbol tree)))]
        [(app-exp? tree)
         (apply-proc
          (eval-exp (app-exp-proc tree) e)
          (map (lambda (exp) (eval-exp exp e)) (app-exp-args tree)))]
        [(ite-exp? tree)
         (eq? 'False (eval-exp (ite-exp-if tree) e))
             (eval-exp (ite-exp-else tree) e)
             (eval-exp (ite-exp-then tree) e)]
        [(let-exp? tree)
         (eval-exp (let-exp-body tree)
                   (env (let-exp-symbols tree)
                        (map (λ (exp) (eval-exp exp e))
                             (let-exp-tree tree))
                        e))]
        [(lambda-exp? tree)
         (closure (lambda-exp-params tree)
                  (lambda-exp-body tree) e)]
        [(set-exp? tree)
         (set-box! (env-lookup e (set-exp-sym tree))
                   (eval-exp (set-exp-val tree) e))]
        [(begin-exp? tree)
         (foldl (lambda (exp acc) (eval-exp exp e))
                (void)
                (begin-exp-lst tree))]
        [else (error 'eval-exp "Invalid tree: ~s" tree)]))

(define (prim-proc symbol)
  (list 'prim-proc symbol))

(define (prim-proc? value)
  (if (equal? (first value) 'prim-proc)
      #t
      #f))

(define (prim-proc-op value)
  (second value))

(define primitive-operators '(+ - * / add1 sub1 negate list cons car cdr number? eqv? lt? gt? leq? geq? null? list?))

(define prim-env
  (env primitive-operators
       (map prim-proc primitive-operators)
       empty-env))

(define init-env
  (env '(x y null True False)
       '(23 42 () True False)
       prim-env))

(define (apply-proc proc args)
  (cond [(prim-proc? proc)
         (apply-primitive-op (prim-proc-op proc) args)]
        [(closure? proc)
         (eval-exp (closure-body proc)
                   (env (closure-params proc)
                        args
                        (closure-env proc)))]
        [else (error 'apply-proc "bad procedure: ~s" proc)]))

(define (apply-primitive-op op args)
  (cond [(eq? op '+) (apply + args)]
        [(eq? op '-) (apply - args)]
        [(eq? op '*) (apply * args)]
        [(eq? op '/) (apply / args)]
        [(eq? op 'add1) (apply add1 args)]
        [(eq? op 'sub1) (apply sub1 args)]
        [(eq? op 'negate) (apply (λ (x)
                                   (- 0 x))
                                 args)]
        [(eq? op 'list) args]
        [(eq? op 'car) (car (first args))]
        [(eq? op 'cdr) (cdr (first args))]
        [(eq? op 'cons) (cons (first args))]
        [(eq? op 'eqv?) (if (apply eqv? args) 'True 'False)]
        [(eq? op 'lt?) (if (apply < args) 'True 'False)]
        [(eq? op 'gt?) (if (apply > args) 'True 'False)]
        [(eq? op 'leq?) (if (apply <= args) 'True 'False)]
        [(eq? op 'geq?) (if (apply >= args) 'True 'False)]
        [(eq? op 'number?) (if (number? (first args)) 'True 'False)]
        [(eq? op 'null?) (if (null? (first args)) 'True 'False)]
        [(eq? op 'list?) (if (list? (first args)) 'True 'False)]
        [else (error 'apply-primitive-op "Unknown primitive: ~s" op)]))

(define (closure parameter-list body environment)
  (list 'closure parameter-list body environment))

(define (closure? obj)
      (if (eq? 'closure (first obj))
          #t
          #f))

(define (closure-params c)
  (second c))

(define (closure-body c)
   (third c))

(define (closure-env c)
   (fourth c))