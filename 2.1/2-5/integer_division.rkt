#lang racket

(define (integer_division n)
  (define (integer_division_inner n m) ;the division elements of n is not bigger then m, return value is the kinds of division
    (if (or (< n 1) (< m 1))
        0
        (if (or (= n 1) (= m 1))
            1
            (if (< n m)
                (integer_division_inner n n)
                (if (= n m)
                    (+ 1 (integer_division_inner n (- m 1)))
                    (+ (integer_division_inner n (- m 1)) (integer_division_inner (- n m) m))
                    )
                )
            )
        )
    )
  (integer_division_inner n n)
  )


;******************************************************test part**************************************************************
(integer_division 1)
(integer_division 3)
(integer_division 5)
(integer_division 10)