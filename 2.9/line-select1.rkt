#lang racket
(define (partition vec start end)
  (let* ([stop -1] ;used to identifying the stop position of partition
         [ind (+ (random (add1 (- end start))) start)]
         [standard (vector-ref vec ind)])
    (vector-set! vec ind (vector-ref vec start)) ;swap the random position's value to start, as the standard of partition
    (vector-set! vec start standard)
    (let loop ([i start] [j end]) ;loop stop when i>=j
      (if (>= i j) 
          (set! stop i)
          (begin
            (let loop1 () ;loop1 stop when i==j or right part of the value in location j smaller than standard
              (cond
                [(= i j) (void)]
                [(< (vector-ref vec j) standard)
                 (vector-set! vec i (vector-ref vec j)) ;put the smaller value to the left part, needn't use swap, because the standard value is always the value in the i or j stoped position
                 (set! i (add1 i))] ;value in location i is <= standard already, so add1
                [else
                 (set! j (sub1 j))
                 (loop1)]))
            (let loop2 () ;loop2 stop when i==j or left part of the value in location i bigger than standard
              (cond
                [(= i j) (void)]
                [(> (vector-ref vec i) standard)
                 (vector-set! vec j (vector-ref vec i)) ;put the big value to the right part, needn't use swap, because the standard value is always the value in the i or j stoped position
                 (set! j (sub1 j))] ;value in location j is >= standard already, so sub1
                [else
                 (set! i (add1 i))
                 (loop2)]))
            (loop i j)
            )
          )
      )
    (vector-set! vec stop standard) ;set the standard value in the final stopped position
    stop
    )
  )

(define (line-select vec k) ;k here is 1-based, orient human
  (set! k (sub1 k)) ;k here is 0-based, orient machine
  (let line-select-inner ([start 0]
                          [end (sub1 (vector-length vec))])
    (if (>= start end)
        (vector-ref vec start)
        (let ([stop (partition vec start end)]) ;partitine stop at stop position
          (cond
            ([= k stop] (vector-ref vec stop)) ;value at stop is the value we want
            {[< k stop] (line-select-inner start (sub1 stop))} ;find in left partition, value at stop is surely not the value you want
            {[> k stop] (line-select-inner (add1 stop) end)} ;find in right partition, value at stop is surely not the value you want
            )
          )
        )
    )
  )
                        
              
           