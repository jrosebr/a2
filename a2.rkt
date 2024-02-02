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

(define vars
  (λ (exp)
    (match exp
      [`,y #:when (symbol? y) (list y)]
      [`(lambda (,x) ,body) #:when (symbol? x) (vars body)]
      [`(,rator ,rand) (append (vars rator) (vars rand))])))

;Problem 8
(define unique-vars
  (λ (exp)
    (match exp
      [`,y #:when (symbol? y) (list y)]
      [`(lambda (,x) ,body) #:when (symbol? x) (union '() (unique-vars body))]
      [`(,rator ,rand) (if (eqv? rator rand)
                           ;then
                           (unique-vars rator)
                           ;else
                           (union (unique-vars rator) (unique-vars rand)))]
      [`else '()])))

;Problem 9
(define var-occurs-free?
  (λ (x exp)
    (match exp
      [`,y #:when (symbol? y) (eqv? x y)]
      [`(lambda (,y) ,body) #:when (symbol? y) (and (var-occurs-free? x body) (not (eqv? x y)))]
      [`(,rator ,rand) (or (var-occurs-free? x rator) (var-occurs-free? x rand))]
      [`else #f])))

;Problem 10
(define var-occurs-bound?
  (λ (x exp)
    (match exp
      [`,y #:when (symbol? y) #f]
      [`(lambda (,y) ,body) #:when (symbol? y) (or (var-occurs-bound? x body) (and (var-occurs-free? x body) (eqv? x y)))]
      [`(,rator ,rand) (or (var-occurs-bound? x rator) (var-occurs-bound? x rand))])))

;Problem 11
(define unique-free-vars
  (λ (exp)
    (define (collect-free-vars exp bound-vars)
      (match exp
        [`,x #:when (symbol? x)
              (if (and (symbol? x) (not (member x bound-vars)))
                  ;then
                  (list x)
                  ;else
                  '())]
        [`(lambda (,y) ,body) #:when (symbol? y)
              (collect-free-vars body (cons y bound-vars))]
        [`(,rator ,rand)
              (remove-duplicates (append (collect-free-vars rator bound-vars)
                                          (collect-free-vars rand bound-vars)))]
        [`else '()]))
    
    (remove '() (collect-free-vars exp '()))))

;Problem 12
(require racket/trace)
(define unique-bound-vars
  (λ (exp)
    (match exp
      [`,y 
  (trace unique-bound-vars)
    