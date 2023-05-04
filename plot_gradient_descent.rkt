#lang racket
(require plot)
(require math/flonum)
(require "gradient_descent.rkt")

(module+ main
  (require "pizza_data.rkt")
  (define data (load-data))
  (define-values (X Y) (reservations-and-pizzas data))
  (trace-train X Y 3 0.001)
  (plot3d (surface3d (lambda (w b) (loss X Y (fl w) (fl b))) -10.0 10.0 -20.0 20.0 #:label "Loss" #:color 1)
          #:x-label "Weight"
          #:y-label "Bias"))