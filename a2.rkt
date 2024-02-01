#lang racket

;Problem 1
(define list-ref
  (lambda (ls n)
    (letrec
        ([nth-cdr (lambda (ls)
                    (lambda (n)
                      (if (or (null? ls) (<= n 0))
                          ls
                          ((nth-cdr (cdr ls)) (- n 1)))))])
      (car ((nth-cdr ls) n)))))

;Problem 2
(define union
  (λ (ls1 ls2)
    (cond
      ((empty? ls1) ls2)
      ((empty? ls2) ls1)
      ((memv (car ls1) ls2)  (union ls1 (cdr ls2)))
      (else (cons (car ls1) (union (cdr ls1) ls2))))))

;Problem 3
(define stretch
  (λ (pred x)
   (λ (value)
     (or (eqv? value x) (pred value)))))

;Problem 4
(define walk-symbol
  (λ (x a_list)
    (let ((result (assv x a_list)))
      (if result
          ;then
          (let ((value (cdr result)))
            (if (symbol? value)
                (walk-symbol value a_list)
                value))
          ;else
          x))))

;Problem 5
(define lambda-exp?
  (λ (E)
    (letrec
      ([p
        (λ (e)
          (match e
            [`,y #t]
            [`(lambda (,x) ,body) (p body)]
            [`(,rator ,rand . ,more) (or (p rator) (p rand))]
            [else #f]))])
      (p E))))