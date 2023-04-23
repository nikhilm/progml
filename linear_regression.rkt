#lang typed/racket
(require math/array)
(require math/flonum)
(require plot)

(: load-data (-> (Array Float)))
(define (load-data)
  (list*->array
   (map (lambda ([s : String]) : (Listof Float)
          (cast (map fl (cast (map string->number (string-split s)) (Listof Integer))) (Listof Float)))
        (rest (with-input-from-file "pizza.txt"
                port->lines)))
   flonum?))

(: as-X-and-Y ((Array Float) -> (Values (Array Float) (Array Float))))
(define (as-X-and-Y data)
  (let [(swapped (array-axis-swap data 0 1))]
    (values (array-slice-ref swapped (list 0 ::...))
            (array-slice-ref swapped (list 1 ::...)))))


(: pizza-data-base-renderer ((Array Float) -> renderer2d))
(define (pizza-data-base-renderer data)
  (points (cast (array->vector* data) (Sequenceof (Sequenceof Float)))
          #:color "Firebrick"))

(: predict ((Array Float) Float Float -> (Array Float)))
(define (predict X w b)
  (array+ (array* X (array w)) (array b)))

(: loss ((Array Float) (Array Float) Float Float -> Float))
(define (loss X Y w b)
  (let ([squared-error (array-sqr (array- (predict X w b) Y))])
    (/ (array-all-sum squared-error) (array-size squared-error))))

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

(: train ((Array Float) (Array Float) Integer Float -> (Values Float Float)))
(define (train X Y iterations lr)
  (let loop : (Values Float Float)
    ([w 0.0] [b 0.0] [iter iterations])
    (define current-loss (loss X Y w b))
    (cond
      [(zero? iter) (error "did not converge!")]
      [(< (loss X Y (+ w lr) b) current-loss) (loop (+ w lr) b (sub1 iter))]
      [(< (loss X Y (- w lr) b) current-loss) (loop (- w lr) b (sub1 iter))]
      [(< (loss X Y w (+ b lr)) current-loss) (loop w (+ b lr) (sub1 iter))]
      [(< (loss X Y w (- b lr)) current-loss) (loop w (- b lr) (sub1 iter))]
      [else (values w b)])))
  
(define (main)
  (let ([data (load-data)])
    (define-values (X Y) (as-X-and-Y data))
    (define-values (w b) (train X Y 10000 0.01))
    (printf "w=~v b=~v~n" w b)
    (printf "Number of pizzas for ~v reservations: ~v~n" 20 (predict (array 20.0) w b))
    #;(pizza-plot data w b)))

(main)