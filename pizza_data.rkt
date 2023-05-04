#lang typed/racket
(require math/array)
(require math/flonum)
(require math/matrix)

(provide load-data
         reservations-and-pizzas)

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

