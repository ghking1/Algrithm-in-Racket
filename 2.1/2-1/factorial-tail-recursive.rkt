#lang racket

(define (factorial n)
  (define fun (lambda (n product)
                (if (= n 0)
                    product
                    (fun (- n 1) (* product n))
                )
              )
  )
  (fun n 1)
)