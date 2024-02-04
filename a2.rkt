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
      ((memv (car ls1) ls2)  (union (cdr ls1) ls2))
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
      [`,y #:when (symbol? y) (equal? x y)]
      [`(lambda (,y) ,body) #:when (symbol? y) (var-occurs? x body)]
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
      (match exp
        [`,x #:when (symbol? x) (list x)]
        [`(lambda (,y) ,body) #:when (symbol? y)
                              (remove y (unique-free-vars body))]
        [`(,rator ,rand)
              (union (unique-free-vars rator) (unique-free-vars rand))])))

;Problem 12
(require racket/trace)
(define unique-bound-vars
  (λ (exp)
      (match exp
        [`,x #:when (symbol? x) '()]
        [`(lambda (,y) ,body) #:when (symbol? y)
                              (if (var-occurs-free? y body)
                                  (cons y (unique-bound-vars body))
                                  (unique-bound-vars body))]
        [`(,rator ,rand)
              (union (unique-bound-vars rator) (unique-bound-vars rand))])))
(trace unique-bound-vars)
;(display (unique-bound-vars 'x))
;(display (unique-bound-vars '(lambda (x) y)))
;(display (unique-bound-vars '(lambda (x) (x y))))
;(display (unique-bound-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c))))))))
;(display (unique-bound-vars '(lambda (y) y)))
;(display (unique-bound-vars '(lambda (x) (y z))))
;(display ((unique-bound-vars '(lambda (x) (lambda (x) x)))))

;Problem 13
(define index
  (λ (var ls)
    (match ls
      (`() (error "not found"))
      (`(,a . ,_) #:when (eqv? var a) 0)
      (`(,_ . ,d) (add1 (index var d))))))

(define lex
  (λ (e ls)
    (match e
      (`,y #:when (symbol? y) (index y ls))
      (`(λ (,x) ,body)
       #:when (symbol? x)
       `(λ ,(lex body (cons x ls))))
      (`(,rator ,rand)
       `(,(lex rator ls) ,(lex rand ls))))))

(define e1=e2?
  (λ (e1 e2)
    (match `(,e1 ,e2)
      (`(,y1 ,y2)
       #:when (and (symbol? y1) (symbol? y2))
       (equal? y1 y2))
      (`((λ (,x1) ,b1) (λ (,x2) ,b2))
       #:when (and (symbol? x1) (symbol? x2))
            (equal? (lex `(λ (,x1) ,b1) '()) (lex `(λ (,x2) ,b2) '())));VERY IMPORTANT
      (`((,rator1 ,rand1) (,rator2 ,rand2))
       (and (e1=e2? rator1 rator2)
            (e1=e2? rand1 rand2))))))