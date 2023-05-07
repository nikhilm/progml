#lang racket
(require flomat)
(require "pizza_3_data_flomat.rkt")

(define (matrix->only-element M)
  (unless (and (equal? (nrows M) (ncols M)) (equal? 1 (nrows M)))
    (raise-arguments-error 'matrix->only-element
                           "must be a square matrix of size 1"
                           "input size" (list (nrows M) (ncols M))))
  (ref M 0 0))

(define (predict X w)
  (times X w))

(define (matrix-avg M)
  (define array-sum (for*/sum ([row (in-range (nrows M))]
                               [col (in-range (ncols M))])
                      (ref M row col)))
  (/ array-sum (size M)))

(define (loss X Y w)
  (define squared-error
    (.sqr! (.- (predict X w) Y)))
  (matrix-avg squared-error))

(define (gradient X Y w)
  (define mult (times (transpose X) (.- (predict X w) Y)))
  (./ (.* mult 2.0) (nrows X)))

(define (train X Y iterations lr)
  (for/fold
   ([w (zeros (ncols X) 1)])
   ([i (in-range iterations)])
    #;(printf "Iteration ~v => Loss: ~v~n" i (loss X Y w))
    (.- w (.* (gradient X Y w) lr))))

(module+ main
  (define-values (parameters pizzas) (parameters-and-pizzas (load-data #:skip-rows 1)))
  (define parameters-with-bias
    (augment 
     (ones (nrows parameters) 1)
     parameters))
  (define w (train parameters-with-bias pizzas 100000 0.001))
  (printf "Final weights are:~n~v~n" w)
  (printf "Some predictions:~n")
  (for ([row-index (in-range 5)])
    (define prediction (predict (row parameters-with-bias row-index) w))
    (printf "X[~v] -> ~v (ground truth: ~v)~n"
            row-index
            (matrix->only-element prediction)
            (matrix->only-element (row pizzas row-index)))))