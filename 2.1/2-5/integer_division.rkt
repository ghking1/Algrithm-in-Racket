#lang racket

(define (integer_division n m)
  (if (or (< n 1) (< m 1))
      0
      (if (or (= n 1) (= m 1))
          1
          (if (< n m)
              (q n n)
              (if (= n m)
                  (+ 1 (q n (- m 1)))
                  (+ (q n (- m 1)) (q (- n m) m))
                  )
              )
          )
      )
  )
