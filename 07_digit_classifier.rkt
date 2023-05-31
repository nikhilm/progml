#lang racket/base
(require flomat)
(require "load-mnist.rkt")

(define (matrix->only-element M)
  (unless (and (equal? (nrows M) (ncols M)) (equal? 1 (nrows M)))
    (raise-arguments-error 'matrix->only-element
                           "must be a square matrix of size 1"
                           "input size" (list (nrows M) (ncols M))))
  (ref M 0 0))

(define (sigmoid! M)
  (./! (.+! (.exp! (.-! M)) 1)))

(define (forward X w)
  (sigmoid! (times X w)))

(define-pointwise-unary round)

(define (max-row-index M)
  ; TODO: do better than using set!
  (define max-idx 0)
  (define max-val (ref M 0 0))
  #;(printf "Figuring out row max for row ~v~n" M)
  (for ([i (in-range (ncols M))])
    #;(printf "Current max (~v / ~v). ~v: ~v~n" max-idx max-val i (ref M 0 i))
    (when (> (ref M 0 i) max-val)
        (set! max-val (ref M 0 i))
      (set! max-idx i)))
  #;(printf "Max idx is ~v~n" max-idx)
  max-idx)

(define (max-index-per-row M)
  (define result (zeros (nrows M) 1))
  (for ([i (in-range (nrows M))])
    (define row (row! M i))
    (mset! result i 0 (max-row-index row))
    #;(error "OOPS"))
  result)

(define (classify X w)
  (define y-hat (forward X w))
  (define labels (max-index-per-row y-hat))
  #;(printf "~n")
  #;(flomat-print (sub labels 0 0 50 1) (current-output-port) #f)
  labels)

(define (matrix-row-avg M)
  (define array-sum (for*/sum ([row (in-range (nrows M))]
                               [col (in-range (ncols M))])
                      (ref M row col)))
  (/ array-sum (nrows M)))

(define (one-minus n)
  (- 1 n))

(define-pointwise-unary one-minus)
(define-pointwise-binary =)

(define (loss X Y w)
  (define y_hat (forward X w))
  (define first_term (.* Y (.log y_hat)))
  (define second_term (.*! (.one-minus Y) (.log! (.one-minus y_hat))))
  (- (matrix-row-avg (.+! first_term second_term))))

(define (gradient X Y w X_T)
  (define mult (times X_T (.-! (forward X w) Y)))
  (./! mult (nrows X)))

(define (train X Y iterations lr)
  (define X_T (transpose X))
  (for/fold
   ([w (zeros (ncols X) (ncols Y))])
   ([i (in-range iterations)])
    (printf "Iteration ~v => Loss: ~v~n" i (loss X Y w))
    (.-! w (.*! (gradient X Y w X_T) lr))))

(define (test X Y w)
  (define total_examples (nrows X))
  (define classified (classify X w))
  (define success (for*/sum ([row (in-range (nrows classified))]
             [col (in-range (ncols classified))])
    (if (= (ref classified row col) (ref Y row col)) 1 0)))
  (define success_pct (* 100.0 (/ success total_examples)))
  (printf "Success rate: ~v%~n" success_pct))

(module+ main
  (define train-images (prepend-bias (read-mnist-images "mnist-data/train-images-idx3-ubyte.gz")))
  (define train-labels (one-hot-encode (read-mnist-labels "mnist-data/train-labels-idx1-ubyte.gz")))
  (define test-images (prepend-bias (read-mnist-images "mnist-data/t10k-images-idx3-ubyte.gz")))
  (define test-labels (read-mnist-labels "mnist-data/t10k-labels-idx1-ubyte.gz"))
  (printf "LOADING DONE~n")
  (define w (train train-images train-labels 200 1e-5))
  (printf "weights ~v~n" w)
  (test test-images test-labels w))