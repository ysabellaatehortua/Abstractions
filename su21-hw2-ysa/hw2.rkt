#lang racket

; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt
(provide (all-defined-out))

; Number One

(define (merge lst1 lst2)
  (cond
    ((null? lst1) lst2)
    ((null? lst2) lst1)
    (else
        (if (<= (first lst1) (first lst2))
          (cons
            (first lst1)
            (merge (rest lst1) lst2))
          (cons (first lst2) (merge lst1 (rest lst2)))))
    ))

; Number Two

(define (insert x lst)
    (cond
        [(empty? lst) (cons x lst)] 
        [(<= x (first lst)) (cons x lst)] 
        (else 
            (cons (first lst) (insert x (rest lst)))
            )))

(define (sort lst)
    (cond
        [(empty? lst) lst]
        (else (insert (first lst) (sort (rest lst)))
            )))

; Number Three

(define (starts-with? sublist biglist)
  (cond
    [(empty? sublist) #t]
    [(equal? (first sublist) (first biglist))
     (starts-with? (rest sublist) (rest biglist))]
    [else #f]
    ))

(define (contains-sublist sublist biglist)
  (cond
    [(empty? biglist) (empty? sublist)]
    [(starts-with? sublist biglist) #t]
    [(contains-sublist sublist (rest biglist))]
    [else #f]
    ))

; Number Four

(define (remove-sublist sublist biglist)
  (cond
    [(null? biglist) null]
    [(null? sublist) biglist]
    [(starts-with? sublist biglist)
     (remove-sublist (rest sublist) (rest biglist))]
    [else (cons (first biglist)
                 (remove-sublist sublist (rest biglist)))]
    ))

; Phonebook

(define phone-book
  '((barbara 775-1234)
    (luke 774-2839)
    (nick 775-0912)
    (valerie 775-9043)))

; Number Five

(define (phone-number person phone-book)
  (cond
    [(empty? phone-book) 'disconnected]
    [(equal? (first (first phone-book)) person) (rest (first phone-book))]
    [else (phone-number person (rest phone-book))]))

; Number Six

(define (person phone-number phone-book)
  (cond
    [(empty? phone-book) 'disconnected]
    [(equal? (second (first phone-book)) phone-number) (first (first phone-book))]
    [else (person phone-number (rest phone-book))]))
    
 
; Number Seven

(define deepen
  (lambda (lst)
    (if (null? lst)
        (list)
        (cons (list (car lst)) (deepen (cdr lst))))))

; Number Eight

(define (eval-bin lst acc)
  (cond
    [(empty? lst) acc]
    [(eval-bin (rest lst) (+ (first lst) (* acc 2)))]
    ))

; Number Nine

(define (sub old new lst)
    (cond
      ((or (null? lst)) lst)
    ((equal? (first lst) old)
     (cons new (sub old new (rest lst))))
    (else
     (cons (first lst) (sub old new (rest lst))))))


; Number Ten

(define (replace old-lst new-lst x)
  (cond
    [(empty? old-lst) x]
    [(equal? (first old-lst) x) (first new-lst)]
    [else (replace (cdr old-lst) (cdr new-lst) x)]
    ))

(define (subs old-lst new-lst lst)
  (cond
    [(empty? lst) lst]
    [else (cons (replace old-lst new-lst (first lst)) (subs old-lst new-lst (rest lst)))]
    ))

