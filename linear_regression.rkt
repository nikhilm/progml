#lang typed/racket
(require math/array)
(require math/flonum)
(require math/matrix)

(provide train
         predict
         loss)


(: predict ((Array Float) Float Float -> (Array Float)))
(define (predict X w b)
  (array+ (array-scale X w) (array b)))

(: loss ((Array Float) (Array Float) Float Float -> Float))
(define (loss X Y w b)
  (let ([squared-error (array-sqr (array- (predict X w b) Y))])
    (/ (array-all-sum squared-error) (array-size squared-error))))


(: train ((Array Float) (Array Float) Integer Float -> (Values Float Float)))
(define (train X Y iterations lr)
  (let loop : (Values Float Float)
    ([w 0.0] [b 0.0] [iter iterations])
    (define current-loss (loss X Y w b))
    (define iter-1 (sub1 iter))
    #;(printf "current w ~v loss ~v~n" w current-loss)
    (cond
      [(zero? iter) (error "did not converge!")]
      [(< (loss X Y (+ w lr) b) current-loss) (loop (+ w lr) b iter-1)]
      [(< (loss X Y (- w lr) b) current-loss) (loop (- w lr) b iter-1)]
      [(< (loss X Y w (+ b lr)) current-loss) (loop w (+ b lr) iter-1)]
      [(< (loss X Y w (- b lr)) current-loss) (loop w (- b lr) iter-1)]
      [else (values w b)])))


(module+ main
  (require "pizza_data.rkt")
  (define data (load-data))
  (define-values (X Y) (reservations-and-pizzas data))
  (define-values (w b) (train X Y 10000 0.01))
  (printf "w=~v b=~v~n" w b)
  (printf "Number of pizzas for ~v reservations: ~v~n" 20 (array-ref (predict (array 20.0) w b) #())))