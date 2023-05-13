#lang racket/base
(require flomat)
(require "police_data.rkt")

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

(define (classify X w)
  (.round (forward X w)))

(define (matrix-avg M)
  (define array-sum (for*/sum ([row (in-range (nrows M))]
                               [col (in-range (ncols M))])
                      (ref M row col)))
  (/ array-sum (size M)))

(define (one-minus n)
  (- 1 n))

(define-pointwise-unary one-minus)
(define-pointwise-binary =)

(define (loss X Y w)
  (define y_hat (forward X w))
  (define first_term (.* Y (.log y_hat)))
  (define second_term (.*! (.one-minus Y) (.log! (.one-minus y_hat))))
  (- (matrix-avg (.+! first_term second_term))))

(define (gradient X Y w X_T)
  (define mult (times X_T (.-! (forward X w) Y)))
  (./! mult (nrows X)))

(define (train X Y iterations lr)
  (define X_T (transpose X))
  (for/fold
   ([w (zeros (ncols X) 1)])
   ([i (in-range iterations)])
    #;(printf "Iteration ~v => Loss: ~v~n" i (loss X Y w))
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
  (define-values (parameters police) (parameters-and-police (load-data #:skip-rows 1)))
  (define parameters-with-bias
    (augment 
     (ones (nrows parameters) 1)
     parameters))
  (define w (train parameters-with-bias police 10000 0.001))
  (printf "weights ~v~n" w)
  (test parameters-with-bias police w))