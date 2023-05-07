#lang racket
(require flomat)

(provide load-data
         parameters-and-pizzas)

(define (load-data #:skip-rows [to-skip 0])
  (with-input-from-file "pizza_3_vars.txt"
    (lambda ()
      ; Discard to-skip
      (for ([_ (in-range to-skip)]) (read-line))

      ; Determine number of columns and set up structures.
      (define a-line (read-line))
      (define tokens (string-split a-line))
      (define n-cols (length tokens))
      (define numbers
        (for/list ([r (in-sequences
                     (with-input-from-string a-line in-port)
                     (in-port))])
          (real->double-flonum r)))
      (define n-rows ((length numbers) . / . n-cols))
      ; Unclear what logic reshape uses,
      ; otherwise it would be nice to use (reshape n-cols n-rows (matrix numbers))
      (apply flomat/dim (append (list n-rows n-cols) numbers)))))

(define (parameters-and-pizzas df)
  (define n-cols (ncols df))
  (values (sub df 0 0 (nrows df) (sub1 n-cols)) (col df (sub1 n-cols))))

(module+ main
  (define-values (params pizzas) (parameters-and-pizzas (load-data #:skip-rows 1)))
  (parameterize ([current-max-flomat-print-size 200])
    (flomat-print params (current-output-port) #t)
    (flomat-print pizzas (current-output-port) #t)))