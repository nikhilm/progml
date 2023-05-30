#lang racket/base
(require file/gunzip)
(require racket/match)
(require flomat)

(provide read-mnist-images
         read-mnist-labels
         prepend-bias
         encode-fives)

(define (bytes->mnist-i32 bstr [start 0])
  (integer-bytes->integer
   bstr
   #t ; signed
   #t ; big-endian
   start
   (+ start 4)))

(define (read-mnist-images-header)
  ; the header is 4 integers
  (map bytes->mnist-i32 (build-list 4 (λ (_) (read-bytes 4)))))

(define (read-mnist-labels-header)
  ; the header is 4 integers
  (map bytes->mnist-i32 (build-list 2 (λ (_) (read-bytes 4)))))

(define (with-input-from-gunzip-port thunk)
  (define-values (read-end write-end) (make-pipe (* 4 1024 1024) 'gunzip-read-port 'gunzip-write-port))
  (define gunzip-thread
    (thread
     (lambda ()
       (gunzip-through-ports (current-input-port) write-end)
       (close-output-port write-end))))
  (begin0
    (parameterize ([current-input-port read-end])
      (thunk))
    (close-input-port read-end)
    (thread-wait gunzip-thread)))

(define (with-input-from-gunzip-file path thunk)
  (with-input-from-file path
    (lambda ()
      (with-input-from-gunzip-port thunk))))

(define (read-mnist-images path)
  (time (with-input-from-gunzip-file path
    (lambda ()
      (match-define (list magic n-items n-rows n-cols) (read-mnist-images-header))
      (unless (eq? magic 2051)
        (error 'read-mnist-images "Not a MNIST image file. Expected magic number ~v, got ~v" 2051 magic))
      
      (define v (build-vector (* n-items n-rows n-cols)
                              (lambda (_) (read-byte))))
      (vector->flomat n-items (* n-rows n-cols) v)))))


(define (read-mnist-labels path)
  (with-input-from-gunzip-file path
    (lambda ()
      (match-define (list magic n-items) (read-mnist-labels-header))
      (unless (eq? magic 2049)
        (error 'read-mnist-labels "Not a MNIST image file. Expected magic number ~v, got ~v" 2049 magic))
      (define v (build-vector n-items (lambda (_) (read-byte))))
      (vector->flomat n-items 1 v))))

(define (prepend-bias M)
  (augment (ones (nrows M) 1) M))

(define (is-5 x)
  (if (= 5 x) 1.0 0.0))

(define-pointwise-unary is-5)
(define (encode-fives M)
  (.is-5 M))
  
#;
(require ffi/unsafe)
#;
(require ffi/cvector)
#;
(time (make-cvector _double (* 60000 28 28)))
#;
(time
 (define sz (* 60000 784))
 (define mm (alloc-flomat 60000 784))
 (define ffi-memset (get-ffi-obj "memset" #f (_fun _pointer _int _int -> _pointer)))
 (ffi-memset mm 0 (* sz 8))
 #;(memset mm 0 0 sz _double))

; These implementations are really slow because of the multiple reads/copies/float conversions and ffi operations somehow being slow.
; What we actually want to do is:
; 1. After reading the byte strings, see if we can get a reference to the underlying byte string, otherwise copy
; 2. Allocate using alloc-flomat
; 3. Iterate over and fill in the _flomat using direct (ptr-elm) based access into the float buffer and the C FFI.
; 4. Create a flomat using the flomat struct constructor and pass it the _flomat.
; In the worst case, the underlying buffer is ~370MB.
; 5. Remember to correctly handle the fact that this expects a row-major representation.
(module+ main
  #;(printf "~v mnist images~n" (shape (read-mnist-images "mnist-data/train-images-idx3-ubyte.gz")))
  (printf "~v mnist labels~n" (shape (read-mnist-labels "mnist-data/train-labels-idx1-ubyte.gz")))
  #;(printf "~v mnist images~n" (shape (read-mnist-images "mnist-data/t10k-images-idx3-ubyte.gz")))
  (printf "~v mnist labels~n" (shape (read-mnist-labels "mnist-data/t10k-labels-idx1-ubyte.gz"))))


#|
3 x 2 matrix
1 2
3 4
5 6

is stored as 1 3 5 2 4 6 in memory
so that (index 2 0) (val 5) is 2 (+ i (* j 3)
so that (index 2 1) (val 6) is 5
|#