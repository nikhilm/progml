Working through "Programming Machine Learning"

Some in Racket, some in Python.

## Notes on performance

Untyped linear regression (With rendering disabled)
- direct runs ~6.3s
- after raco make ~5.8s

Typed linear regression (With rendering disabled)
- direct runs ~3.5s
- after raco make ~1.4s
- removing the `plot` import - ~550ms.
which 3.6x slower than Python using numpy. Not bad.
