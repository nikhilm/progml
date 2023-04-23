(require plot)

(: pizza-plot ((Array Float) Float Float -> Any))
(define (pizza-plot data w b)
  (plot (list (pizza-data-base-renderer data)
              (function (lambda (x) (+ (* x w) b))))
        #:title "Pizza distribution"
        #:x-label "Reservations"
        #:y-label "Pizzas"
        #:x-min 0
        #:y-min 0
        #:x-max 30
        #:y-max 70))

(: pizza-data-base-renderer ((Array Float) -> renderer2d))
(define (pizza-data-base-renderer data)
  (points (cast (array->vector* data) (Sequenceof (Sequenceof Float)))
          #:color "Firebrick"))

(pizza-plot data w b)
