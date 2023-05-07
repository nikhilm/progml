#lang typed/racket
(require math/matrix)
(require math/array)
(require math/flonum)
(require "pizza_3_data.rkt")

(: make-matrix-float (Integer Integer Float ->  (Matrix Float)))
(define (make-matrix-float m n v)
  (make-matrix m n v))

(: matrix->only-element ((Matrix Float) -> Float))
(define (matrix->only-element M)
  (unless (equal? 1 (square-matrix-size M))
    (raise-arguments-error 'matrix->only-element
    "must be a square matrix of size 1"
    "input size" (square-matrix-size M)))
  (matrix-ref M 0 0))

(: predict ((Matrix Float) (Matrix Float) -> (Matrix Float)))
(define (predict X w)
  (matrix* X w))

(: matrix-avg ((Matrix Float) -> Float))
(define (matrix-avg M) : Float
  (/ (array-all-sum M) (array-size M)))

(define (loss [X : (Matrix Float)] [Y : (Matrix Float)] [w : (Matrix Float)]) : Float
  (define squared-error
    (matrix-map sqr (matrix- (predict X w) Y)))
  (matrix-avg squared-error))

(: gradient ((Matrix Float) (Matrix Float) (Matrix Float) (Matrix Float) -> (Matrix Float)))
(define (gradient X Y w X_T)
  (define mult (matrix* X_T (matrix- (predict X w) Y)))
  (matrix-scale (matrix-scale mult 2.0) (fl (/ (matrix-num-rows X)))))

(: train ((Matrix Float) (Matrix Float) Exact-Nonnegative-Integer Float . -> . (Matrix Float)))
(define (train X Y iterations lr)
  (define X_T (matrix-transpose X))
  (for/fold
   ([w (make-matrix-float (matrix-num-cols X) 1 (fl 0))])
   ([i (in-range iterations)])
    #;(printf "Iteration ~v => Loss: ~v~n" i (loss X Y w))
    (matrix- w (matrix-scale (gradient X Y w X_T) lr))))

(module+ main
  (define-values (parameters pizzas) (parameters-and-pizzas (load-data #:skip-rows 1)))
  (define parameters-with-bias
    (matrix-augment (list
                     (make-matrix-float (matrix-num-rows parameters) 1 1.0)
                     parameters)))
  (define w (train parameters-with-bias pizzas 100000 0.001))
  (printf "Final weights are:~n~v~n" w)
  (printf "Some predictions:~n")
  (for ([row (in-range 5)])
    (define prediction (predict (matrix-row parameters-with-bias row) w))
    (printf "X[~v] -> ~v (ground truth: ~v)~n"
            row
            (matrix->only-element prediction)
            (matrix->only-element (matrix-row pizzas row)))))