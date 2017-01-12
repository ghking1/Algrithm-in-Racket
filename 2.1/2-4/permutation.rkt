#lang racket

(define (permutate list k m)
  (let [(permutations '()) (count 0)]
    (define (permutate_inner list k m)
      (if (= k m)
          (set! permutations (cons list permutations))
          ((lambda ()
             (define (loop i)
               (if (< i m)
                   (begin (permutate_inner (swap list k i) (+ k 1) m)
                          (loop (+ i 1))
                          )
                   (void)
                   )
               )
             (loop k)
             ))
          )
      )
    (permutate_inner list k m)
    permutations
    )
  )

(define (swap list i j)
  (let [(a '()) (b '())]
    (define (swap_inner list i j fore middle n count)
      (cond
        [(= i j) list]
        [(= count i) (set! a (cons (car list) a))
                     (swap_inner (cdr list) i j fore middle n (+ count 1))
                     ]
        [(= count j) (set! b (cons (car list) b))
                     (swap_inner (cdr list) i j fore middle n (+ count 1))
                     ]
        [(< count i) (swap_inner (cdr list) i j
                                 (cons (car list) fore) middle
                                 n (+ count 1)
                                 )
                     ]
        [(and (> count i) (< count j)) (swap_inner (cdr list) i j
                                                   fore (cons (car list) middle)
                                                   n (+ count 1)
                                                   )
                                       ]
        [(or (empty? list) (> count j)) (append (reverse fore) b (reverse middle) a list)]
        )
      )
    (swap_inner list i j '() '() (length list) 0)
    )
  )