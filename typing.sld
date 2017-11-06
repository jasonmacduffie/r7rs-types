
(define-library (typing)
  ;; The syntax forms exported from this library are meaningless,
  ;; and are intended to document the types of variables and
  ;; procedures.
  ;;
  ;; For portable code, it is suggested to use pipes for the symbol
  ;; |:|, as colons have special meanings in some Scheme
  ;; implementations.
  ;;
  ;; Note that only forms using "define" and "define-record-type"
  ;; are supported, so anonymous functions may be assumed to accept
  ;; and return types <any>.
  ;;
  ;; EXAMPLE USAGE, based on a Typed Racket example:
  ;;
  ;; (|:| point (-> <real> <real> <point>))
  ;; (define-record-type <point>
  ;;   (point x y)
  ;;   point?
  ;;   (x point-x)
  ;;   (y point-y))
  ;;
  ;; (|:| distance (-> <point> <point> <real>))
  ;; (define (distance p1 p2)
  ;;   (sqrt (+ (square (- (point-x pt1) (point-x pt2)))
  ;;            (square (- (point-y pt1) (point-y pt2))))))
  ;;
  ;; (|:| A <real>)
  ;; (define A 100)
  ;;
  ;; (|:| B <real>)
  ;; (define B 200)
  ;; 
  ;; (|:| C <point>)
  ;; (define C (point A B))
  ;;
  ;; etcetera...
  ;;
  (import (scheme base))
  (export |:|)
  (begin
    (define-syntax |:|
      (syntax-rules ()
        ((_ . rest) 0)))))

