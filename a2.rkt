#lang racket

;Problem 1
(define list-ref
  (λ (lst dex)
    (cond
      ((empty? lst)
       (if (> 0 dex)
           ;then
           'bad-data
           ;else
           