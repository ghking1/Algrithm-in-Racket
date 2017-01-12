#lang racket

(define B-find (lambda (vec val)
                 (let B-find-inner ([start 0]
                                    [end (sub1 (vector-length vec))])
                   (cond
                     [(> start end) (void)]
                     [(< val (vector-ref vec (quotient (+ start end) 2)))
                      (B-find-inner start (sub1 (quotient (+ start end) 2)))]
                     [(= val (vector-ref vec (quotient (+ start end) 2)))
                      (quotient (+ start end) 2)]
                     [(> val (vector-ref vec (quotient (+ start end) 2)))
                      (B-find-inner (add1 (quotient (+ start end) 2)) end)]
                     )
                   )
                 )
  )