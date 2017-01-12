#lang racket

(define B-find (lambda (vec val)
                 (let B-find-inner ([start 0]
                                    [end (sub1 (vector-length vec))])
                   (cond
                     [(> start end) (void)] ;hasn't find the value
                     [(< val (vector-ref vec (quotient (+ start end) 2))) ;value is smaller then middle, then find in left part
                      (B-find-inner start (sub1 (quotient (+ start end) 2)))]
                     [(= val (vector-ref vec (quotient (+ start end) 2))) ;finded!
                      (quotient (+ start end) 2)]
                     [(> val (vector-ref vec (quotient (+ start end) 2))) ;value is bigger then middle, then find in right part
                      (B-find-inner (add1 (quotient (+ start end) 2)) end)]
                     )
                   )
                 )
  )

;******************************************************test part**************************************************************
(define v (vector 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
                  23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43
                  44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64
                  65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86
                  87 88 89 90 91 92 93 94 95 96 97 98 99 ))

(B-find v 0)
(B-find v 20)
(B-find v 30)
(B-find v 55)
(B-find v 67)
(B-find v 99)