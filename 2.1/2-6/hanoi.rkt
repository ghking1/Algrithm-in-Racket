#lang racket

(define (hanoi n)
  (define (hanoi-inner n source dest temp)
    (if (> n 0)
        (begin (hanoi-inner (- n 1) source temp dest)
               (display (list source "->" dest))
               (newline)
               (hanoi-inner (- n 1) temp dest source)
               )
        (void)
        )
    )
  (hanoi-inner n "a" "b" "c")
  )


;******************************************************test part**************************************************************
(hanoi 5)
