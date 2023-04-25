#lang typed/racket
(require math/array)
(require math/flonum)

(provide load-data
         train
         predict
         as-X-and-Y
         loss)

; All I want to do is to load a file that looks like
; Reservations  Pizzas
; 13            33
; 2             16
; 14            32
; ...
; as an array of floats. The casts are really ugly here, but are required
; because string->number returns a (U Complex False)
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


(: predict ((Array Float) Float Float -> (Array Float)))
(define (predict X w b)
  (array+ (array* X (array w)) (array b)))

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
    (define-values (w_grad b_grad) (gradient X Y w b))
    (values
     (- w (* w_grad lr))
     (- b (* b_grad lr)))))


(module+ main
  (let ([data (load-data)])
    (define-values (X Y) (as-X-and-Y data))
    (define-values (w b) (train X Y 20000 0.001))
    (printf "w=~v b=~v~n" w b)
    (printf "Number of pizzas for ~v reservations: ~v~n" 20 (predict (array 20.0) w b))))