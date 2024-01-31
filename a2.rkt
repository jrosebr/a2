#lang racket

;Problem 1
(define list-ref
  (lambda (ls n)
    (letrec
      ([nth-cdr (lambda (n)
                  ;; complete the definition
                  )])
      (car (nth-cdr n)))))
           
           