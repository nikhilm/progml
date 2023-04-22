#lang racket
(require math/array)
(require plot)

(define (load-data)
  (list*->array
   (map (lambda (s)
          (map string->number (string-split s)))
        (rest (with-input-from-file "pizza.txt"
                port->lines))) number?))

(define (as-X-and-Y data)
  (let [(swapped (array-axis-swap data 0 1))]
    (values (array-slice-ref swapped (list 0 ::...))
            (array-slice-ref swapped (list 1 ::...)))))

(define (pizza-data-base-renderer data)
  (points (array->vector* data)
          #:color "Firebrick"))

(define (predict X w b)
  (array+ (array* X (array w)) (array b)))

(define (loss X Y w b)
  (let ([squared-error (array-sqr (array- (predict X w b) Y))])
    (/ (array-all-sum squared-error) (array-size squared-error))))

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

(define (train X Y iterations lr)
  ; TODO: Make this more idiomatic (probably recursive)
  (for/fold ([w 0] [b 0] [should-break #f]
                   #:result (values w b))
            ([i (in-range 1 iterations)])
    #:break should-break
    (define current-loss (loss X Y w b))
    #;(printf "Iteration ~v -> Loss ~v~n" i current-loss)
    (cond
      [(< (loss X Y (+ w lr) b) current-loss) (values (+ w lr) b #f)]
      [(< (loss X Y (- w lr) b) current-loss) (values (- w lr) b #f)]
      [(< (loss X Y w (+ b lr)) current-loss) (values w (+ b lr) #f)]
      [(< (loss X Y w (- b lr)) current-loss) (values w (- b lr) #f)]
      [else (values w b #t)])))

(define (main)
  (let ([data (load-data)])
    (define-values (X Y) (as-X-and-Y data))
    (define-values (w b) (train X Y 10000 0.01))
    (printf "w=~v b=~v~n" w b)
    (printf "Number of pizzas for ~v reservations: ~v~n" 20 (predict (array 20) w b))
    (pizza-plot data w b)))

(main)