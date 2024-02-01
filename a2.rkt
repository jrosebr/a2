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
              [`,y #:when (symbol? y) #t]
              [`(lambda (,x) ,body) #:when (symbol? x) (p body)]
              [`(,rator ,rand) (and (p rator) (p rand))]
              [else #f]))])
      (p E))))

;Problem 6
(define var-occurs?
  (λ (x exp)
    (match exp
      [`,y #:when (symbol? y) (eqv? x y)]
      [`(lambda (,x) ,body) #:when (symbol? x) (var-occurs? x body)]
      [`(,rator ,rand) (or (var-occurs? x rator) (var-occurs? x rand))]
      [else #f])))

;;(require racket/trace)
;;& (trace function name)
;Problem 7

(define append
  (λ (ls1 ls2)
    (if (null? ls1)
        ;then
        ls2
        ;else
        (cons (car ls1) (append (cdr ls1) ls2)))))

(require racket/trace)
(define vars
  (λ (exp)
    (match exp
      [`,y #:when (symbol? y) (list y)]
      [`(lambda (,x) ,body) #:when (symbol? x) (vars body)]
      [`(,rator ,rand) (append (vars rator) (vars rand))])))
(trace vars)