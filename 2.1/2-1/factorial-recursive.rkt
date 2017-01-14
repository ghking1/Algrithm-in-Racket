#lang racket

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))
  )
)


;******************************************************test part**************************************************************

(factorial 3)
(factorial 5)
(factorial 10)