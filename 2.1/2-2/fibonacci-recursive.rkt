#lang racket

(define (fibonacci n)
  (if (<= n 1)
      1
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))
  )
)


;******************************************************test part**************************************************************

(fibonacci 3)
(fibonacci 5)
(fibonacci 10)