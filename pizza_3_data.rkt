#lang typed/racket
(require math/array)
(require math/flonum)
(require math/matrix)

(provide load-data
         parameters-and-pizzas)

(define (load-data #:skip-rows [to-skip : Exact-Nonnegative-Integer 0])
  (with-input-from-file "pizza_3_vars.txt"
    (lambda ()
      ; Discard to-skip
      (for ([_ (in-range to-skip)]) (read-line))

      ; Determine number of columns and set up structures.
      (define a-line (assert (read-line) string?))
      (define tokens (string-split a-line))
      (define n-cols (length tokens))
      (define numbers
        (for/list : (Listof Float) ([r (in-sequences
                     (with-input-from-string a-line in-port)
                     (in-port))])
          (real->double-flonum (assert r real?))))
      (define n-rows (assert ((length numbers) . / . n-cols) exact-positive-integer?))
      (list->matrix n-rows n-cols numbers))))

(define (parameters-and-pizzas [df : (Matrix Float)])
  (define n-cols (matrix-num-cols df))
  (values (submatrix df (::) (:: (sub1 n-cols))) (matrix-col df (sub1 n-cols))))

(module+ main
  (define-values (params pizzas) (parameters-and-pizzas (load-data #:skip-rows 1)))
  (printf "Parameters:~v~n~nPizzas:~n~v~n" params pizzas))