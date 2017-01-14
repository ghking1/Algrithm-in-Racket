#lang racket

(define (permutate list)
  (let [(permutations '())] ;permutations used to save all list of permutation
    (define (permutate_inner list start end) ;start and end position for permutate
      (if (= start end)
          (set! permutations (cons list permutations)) ;when recursive to the last element, then the list is a new permutation, cons it to permutations
          ((lambda () ;this lambda and define realized the action like named-let
             (define (loop i)
               (if (< i end)
                   (begin (permutate_inner (swap list start i) (+ start 1) end) ;permutate from strat+1 to end after swap i-th element to the start position
                          (loop (+ i 1)) ;it will generate next serials permutation which use (i+1)-th element as first element
                          )
                   (void)
                   )
               )
             (loop start) ;begain loop from start position
             ))
          )
      )
    (permutate_inner list 0 (length list))
    permutations
    )
  )

(define (swap list i j)  ;swap two element in list, it return a new list
  (let [(a '()) (b '())]
    (define (swap_inner list i j fore middle n count)
      (cond
        [(= i j) list] ;if i==j, there is no need to swap
        [(< count i) (swap_inner (cdr list) i j ;when count<i cons the count-th element to fore
                                 (cons (car list) fore) middle
                                 n (+ count 1)
                                 )
                     ]
        [(= count i) (set! a (cons (car list) a)) ;when count==i set the count-th element to a
                     (swap_inner (cdr list) i j fore middle n (+ count 1))
                     ]
        [(and (> count i) (< count j)) (swap_inner (cdr list) i j ;when i<count<j, cons the count-th element to middle
                                                   fore (cons (car list) middle)
                                                   n (+ count 1)
                                                   )
                                       ]
        [(= count j) (set! b (cons (car list) b)) ;when count==j set the count-th element to b
                     (swap_inner (cdr list) i j fore middle n (+ count 1))
                     ]
        [(or (empty? list) (> count j)) (append (reverse fore) b (reverse middle) a list)] ;when count>j, join fore b middle a remaind-list as final list. but fore and middle should be reversed
        )
      )
    (swap_inner list i j '() '() (length list) 0)
    )
  )


;******************************************************test part**************************************************************

(permutate '(1 2 3))
(permutate '(1 2 3 4 5))