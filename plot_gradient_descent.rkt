#lang racket
(require plot)
(require racket/random)
(require math/flonum)
(require "gradient_descent.rkt")

(define (trace->point closs grad [bump 300])
  (define w (grad-trace-w grad))
  (define b (grad-trace-b grad))
  (list w
        b
        (+ bump (closs w b))))

(module+ main
  (require "pizza_data.rkt")
  (define data (load-data))
  (define-values (X Y) (reservations-and-pizzas data))
  (define traces (trace-train X Y 20000 0.001))
  ; The first few points jump the most, so we want to render those.
  ; The remaining points jump very little, so only sample a small subset of them.
  (define-values (few rest) (vector-split-at traces 7))
  (define samples (append (vector->list few) (random-sample rest 100)))
  (define points (map (curry trace->point (curry loss X Y)) samples))
  (define plots
    (list
     (surface3d (lambda (w b) (loss X Y (fl w) (fl b))) -2.0 5.0 -20.0 20.0 #:label "Loss" #:color 1)
     (points3d points #:color 3)))
  (plot3d plots
          #:x-label "Weight"
          #:y-label "Bias"))