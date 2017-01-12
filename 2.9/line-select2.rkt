#lang racket
(define (partition vec start end [standard (void)]) ;if provid standard, it must be some value in the vector, or it will encounter error
  (if (equal? standard (void)) ;if hasn't provid standard, use random standard
      (let* ([ind (+ (random (add1 (- end start))) start)]
             [tmp (vector-ref vec ind)])
        (set! standard tmp)
        (vector-set! vec ind (vector-ref vec start)) ;swap the random position's value to start, as the standard of partition
        (vector-set! vec start standard))
      (do ([i start (add1 i)]) ;if provid standard, find the posithon fo value standard, then swap the value of start and the found posithon
        ((= standard (vector-ref vec i))
         (let ([tmp (vector-ref vec start)])
           (vector-set! vec start standard)
           (vector-set! vec i tmp))
         )
        )
      )
  (let ([stop -1]) ;used to identifying the stop position of partition
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

(define (Q-sort vec start end)
  (let Q-sort-inner ([start start]
                     [end end])
    (if (>= start end)
        (void)
        (let ([stop (partition vec start end)]) ;partitine stop at stop position
          (Q-sort-inner start (sub1 stop)) ;Q-sort left partition, value at stop is already bigger than left
          (Q-sort-inner (add1 stop) end) ;Q-sort right partition, value at stop is already smaller than right
          )
        )
    )
  )

(define (line-select vec k) ;k here is 1-based, orient human
  (set! k (sub1 k)) ;k here is 0-based, orient machine
  (let line-select-inner ([start 0]
                          [end (sub1 (vector-length vec))]
                          [k k])
    (if (< (- end start) 75)
        (begin (Q-sort vec start end) ;when vector length <75, use Q-sort find the k-th value
               (vector-ref vec k))
        (begin (do ([i 0 (add1 i)]) ;find the middle value of the groups of five element, then put them to the start part of vector
                 ((> i (quotient (- end start 4) 5)) (void)) ; exit loop when i>quotient((end-start-4)/5)
                 (Q-sort vec (+ start (* i 5)) (+ start (* i 5) 4))
                 (let ([tmp (vector-ref vec (+ start (* i 5) 2))])
                   (vector-set! vec (+ start (* i 5) 2) (vector-ref vec (+ start i)))
                   (vector-set! vec (+ start i) tmp)
                   )
                 )
               (let* ([middle (line-select-inner start (+ start (quotient (- end start 4) 5)) (+ start (quotient (- end start 4) 10)))]
                      [stop (partition vec start end middle)])
                 (cond
                   ([= k stop] (vector-ref vec stop)) ;value at stop is the value we want
                   {[< k stop] (line-select-inner start (sub1 stop) k)} ;find in left partition, value at stop is surely not the value you want
                   {[> k stop] (line-select-inner (add1 stop) end k)} ;find in right partition, value at stop is surely not the value you want
                   )
                 )
               )
        )
    )
  )

(define v (vector 0 1 2 3 90 91 92 93 94 95 96 97 4 5 6 7 8 9 10 11
                  25 26 27 28 29 30 31 32 33 34 35 36 12 13 14 15 16
                  17 18 19 20 21 22 23 24 37 38 39 54 55 56 57 58 59 60
                  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
                  81 82 40 41 42 43 44 45 46 47 48 49 50 51 52 53 83 84 85 86 87 88 89 98 99))

(line-select v 10)
(line-select v 20)
(line-select v 30)
(line-select v 55)
(line-select v 78)
(line-select v 100)
