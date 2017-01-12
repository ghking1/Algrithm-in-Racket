#lang racket
(define (partition vec start end)
  (let* ([stop -1] ;used to identifying the stop position of partition
         [ind (+ (random (add1 (- end start))) start)]
         [tmp (vector-ref vec ind)])
    (vector-set! vec ind (vector-ref vec start)) ;swap the random position's value to start, as the standard of partition
    (vector-set! vec start tmp)
    (let loop ([i start] [j end]) ;loop stop when i>=j
      (if (>= i j) 
          (set! stop i)
          (begin
            (let loop1 () ;loop1 stop when i==j or right part of the value in location j smaller than standard
              (cond
                [(= i j) (void)]
                [(<= (vector-ref vec j) tmp)
                 (let ([tmpval (vector-ref vec i)]) ;swap value of i and j, it put the small value to the left part
                   (vector-set! vec i (vector-ref vec j))
                   (vector-set! vec j tmpval))
                 (set! i (add1 i))] ;value in location i is <= standard already, so add1
                [else
                 (set! j (sub1 j))
                 (loop1)]))
            (let loop2 () ;loop2 stop when i==j or left part of the value in location i bigger than standard
              (cond
                [(= i j) (void)]
                [(>= (vector-ref vec i) tmp)
                 (let ([tmpval (vector-ref vec j)]) ;swap value of i and j, it put the big value to the right part
                   (vector-set! vec j (vector-ref vec i))
                   (vector-set! vec i tmpval))
                 (set! j (sub1 j))] ;value in location j is >= standard already, so sub1
                [else
                 (set! i (add1 i))
                 (loop2)]))
            (loop i j)
            )
          )
      )
    stop
    )
  )

(define (Q-sort vec)
  (let Q-sort-inner ([start 0]
                     [end (sub1 (vector-length vec))])
    (if (>= start end)
        (void)
        (let ([stop (partition vec start end)]) ;partitine stop at stop position
          (Q-sort-inner start (sub1 stop)) ;Q-sort left partition, value at stop is already bigger than left
          (Q-sort-inner (add1 stop) end) ;Q-sort right partition, value at stop is already smaller than right
          )
        )
    )
  )

