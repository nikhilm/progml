#lang racket

(require math/array)
(require math/flonum)
(require plot)

(require "linear_regression.rkt")

(define ((line-func slope bias) x)
  (+ (* x slope) bias))

(define (pizza-plot data w b)
  (plot (list (pizza-data-renderer data)
              (function (line-func w b)))
        #:title "Pizza distribution"
        #:x-label "Reservations"
        #:y-label "Pizzas"
        #:x-min 0
        #:y-min 0
        #:x-max 30
        #:y-max 70))

(define (pizza-data-renderer data)
  (points (array->vector* data)
          #:color "Firebrick"))

(module+ main
  (require "pizza_data.rkt")
  (define data (load-data))
  (define-values (X Y) (reservations-and-pizzas data))
  (define-values (w b) (train X Y 10000 0.01))
  (define prediction (array-ref (predict (array 20.0) w b) #()))
  (pizza-plot data w b))
