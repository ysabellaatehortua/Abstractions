#lang racket

(require "TreeDataType.rkt")

; Eamon McKeon and Ysa Atehortua

; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt.

(provide (all-defined-out))

; PART 1

; index

(define (index x lst)
  (second (foldr (λ (head result)
           (let ([current-idx (sub1 (first result))]
                 [found-idx (second result)])
             (if (equal? head x) (list current-idx current-idx)
                 (list current-idx found-idx))))
         (list (length lst) -1) lst)))


; replace

(define (replace a b lst)
  (foldr (λ (head result)
           (if (equal? head a) (cons b result) (cons head result)))
         empty lst))


; weigh

(define bags
  '((duffle 8)
    (garment-bag 2)
    (briefcase 5)
    (valise 7)
    (steamer-trunk 65)))

(define (weigh bags)
  (foldr (λ (bag sum)
           (+ (second bag) sum))
         0 bags))


; heaviest

(define (heaviest bags)
  (first (foldr (λ (bag heaviest)
           (if (>= (second bag) (second heaviest)) bag heaviest))
         '(0 0) bags)))


; PART 2

; child-sum

(define (child-sum t)
  (if (empty-tree? t) 0 (apply + (map second (tree-children t)))))


; all-sum

(define (all-sum t)
  (cond [(empty-tree? t) 0]
        [(not (tree? t)) (+ (all-sum (first t)) (all-sum (rest t)))]
        [(leaf? t) (tree-value t)]
        [else (+ (tree-value t) (all-sum (first (tree-children t)))
                      (all-sum (rest (tree-children t))))]))


; sizeof

(define (sizeof t)
  ;(if (empty-tree? t) 0 (apply + (map second (tree-children t)))))
  (cond [(empty-tree? t) 0]
        [(not (tree? t)) (+ (sizeof (first t)) (sizeof (rest t)))]
        [(leaf? t) 1]
        [else (+ 1 (sizeof (first (tree-children t)))
                      (sizeof (rest (tree-children t))))]))

    
(define (visit-tree f t)
  (cond [(not (tree? t)) #f] 
        [(empty-tree? t) empty-tree] 
        [(leaf? t) (make-tree (f (tree-value t)))]
        [else (tree (f (tree-value t)) (map (lambda (lst) (visit-tree f lst)) (third t)))]))    
    
; height
(define (height t)
  (cond [(empty-tree? t) -1]
        [(not (tree? t)) (max (height (first t)) (height (rest t)))]
        [(leaf? t) 0]
        [else (+ 1 (max (height (first (tree-children t)))
                        (height (rest (tree-children t)))))]))


; preorder
(define (preorder t)
  (cond [(empty-tree? t) empty]
        [(not (tree? t)) (cons (preorder (first t)) (preorder (rest t)))]
        [(leaf? t) (list (tree-value t))]
        [else (apply append (list (tree-value t)) (preorder (first (tree-children t)))
                      (preorder (rest (tree-children t))))]))

; postorder
(define (postorder t)
  (cond [(not (tree? t)) #f] 
        [(empty-tree? t) empty-tree] 
        [else (append (flatten (map postorder (tree-children t))) (list (tree-value t)))]))   

