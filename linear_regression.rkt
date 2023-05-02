#lang typed/racket
(require math/array)
(require math/flonum)
(require math/matrix)

(provide load-data
         train
         predict
         reservations-and-pizzas
         loss)

; All I want to do is to load a file that looks like
; Reservations  Pizzas
; 13            33
; 2             16
; 14            32
; ...
; as an array of floats. The casts are really ugly here, but are required
; because string->number returns a (U Complex False)
(: load-data (-> (Matrix Float)))
(define (load-data)
  
  (define (str->int-list [s : String])
    (cast (map string->number (string-split s)) (Listof Integer)))

  (: ints->floats ((Listof Integer)-> (Listof Float)))
  (define (ints->floats ints) (map fl ints))
  
  ; rest to skip the title lines
  (define lines (rest (file->lines "pizza.txt")))
  

  (: line->data (String -> (Listof Float)))
  (define (line->data line)
    (ints->floats (str->int-list line)))
  
  (list*->matrix (map line->data lines)))

(: reservations-and-pizzas ((Array Float) -> (Values (Array Float) (Array Float))))
(define (reservations-and-pizzas data)
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
  (define data (load-data))
  (define-values (X Y) (reservations-and-pizzas data))
  (define-values (w b) (train X Y 10000 0.01))
  (printf "w=~v b=~v~n" w b)
  (printf "Number of pizzas for ~v reservations: ~v~n" 20 (predict (array 20.0) w b)))