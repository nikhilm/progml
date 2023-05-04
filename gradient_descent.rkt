#lang typed/racket
(require math/array)
(require math/flonum)
(require racket/format)

(provide train
         trace-train
         predict
         loss)

(: predict ((Array Float) Float Float -> (Array Float)))
(define (predict X w b)
  (array+ (array-scale X w) (array b)))

(: loss ((Array Float) (Array Float) Float Float -> Float))
(define (loss X Y w b)
  (let ([squared-error (array-sqr (array- (predict X w b) Y))])
    (/ (array-all-sum squared-error) (array-size squared-error))))

(: array-mean ((Array Float) -> Float))
(define (array-mean arr)
  (/ (array-all-sum arr) (array-size arr)))

(: gradient ((Array Float) (Array Float) Float Float -> (Values Float Float)))
(define (gradient X Y w b)
  (let ([error (array- (predict X w b) Y)])
    (values
     (* (array-mean (array* X error)) 2.0)
     (* (array-mean error) 2.0))))

(: train ((Array Float) (Array Float) Integer Float -> (Values Float Float)))
(define (train X Y iterations lr)
  (for/fold ([w 0.0] [b 0.0])
            ([i (in-range 0 iterations)])
    #;(printf "Iteration ~v => Loss: ~a~n" i (~r (loss X Y w b)))
    (define-values (w_grad b_grad) (gradient X Y w b))
    (values
     (- w (* w_grad lr))
     (- b (* b_grad lr)))))

(struct grad-trace
  ([w : Float]
   [b : Float]
   [w_grad : Float]
   [b_grad : Float]) #:transparent)

(: trace-train ((Array Float) (Array Float) Integer Float -> (Mutable-Vectorof grad-trace)))
(define (trace-train X Y iterations lr)
  (: trace (Mutable-Vectorof grad-trace))
  (define trace (make-vector iterations (grad-trace 0.0 0.0 0.0 0.0)))
  (for/fold ([w 0.0] [b 0.0])
            ([i (in-range 0 iterations)])
    #;(printf "Iteration ~v => Loss: ~a~n" i (~r (loss X Y w b)))
    (define-values (w_grad b_grad) (gradient X Y w b))
    (vector-set! trace i (grad-trace w b w_grad b_grad))
    (values (- w (* w_grad lr))
            (- b (* b_grad lr))))
  trace)

(module+ main
  (require "pizza_data.rkt")
  (define data (load-data))
  (define-values (X Y) (reservations-and-pizzas data))
  (define-values (w b) (train X Y 20000 0.001))
  (printf "w=~v b=~v~n" w b)
  (printf "Number of pizzas for ~v reservations: ~v~n" 20 (array-ref (predict (array 20.0) w b) #())))