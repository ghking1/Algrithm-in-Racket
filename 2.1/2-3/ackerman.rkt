#lang racket

(define (ackerman a b)
  (if (and (= a 1) (= b 0))
      2
      (if (= a 0)
          1
          (if (and (>= a 2) (= b 0))
              (+ a 2)
              (ackerman (ackerman (- a 1) b) (- b 1))
          )
      )
  )
)


;******************************************************test part**************************************************************

(ackerman 2 3)
(ackerman 3 2)
(ackerman 3 3)
