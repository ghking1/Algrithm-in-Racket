#lang racket

(define (table n)
  (define vv (make-vector n)) ;define a 2-dimension vector
  (for ([i (in-range n)]) 
    (vector-set! vv i (make-vector n)) ;set every element of vv to vector
    (vector-set! (vector-ref vv i) 0 i) ;init every first element of every vector in vv to i
    )
  (define (table-inner n start)
    (if (= n 1)
        (void) ;when n==1 return
        (begin (table-inner (/ n 2) start) ;make table for people from start to (start+n/2)
               (table-inner (/ n 2) (+ start (/ n 2)));make table for people from (start+n/2) to n
               (for* ([i (in-range  start (+ start (/ n 2)))] ;copy table from up-left to down-right, and from down-left to up-right
                      [j (in-range (/ n 2))])
                 (vector-set! (vector-ref vv (+ i (/ n 2))) (+ j (/ n 2)) ;copy table from up-left to down-right
                              (vector-ref (vector-ref vv i) j))
                 (vector-set! (vector-ref vv i) (+ j (/ n 2)) ;copy table from down-left to up-right
                              (vector-ref (vector-ref vv (+ i (/ n 2))) j))
                 )
               )
        )
    )
  (table-inner n 0)
  vv
  )
                   
;******************************************************test part**************************************************************
(table 8)
(table 16)
(table 32)
    
    