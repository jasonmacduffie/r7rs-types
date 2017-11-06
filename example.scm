
;; Example to test type checking

(import
  (scheme base)
  (scheme read)
  (scheme write)
  (scheme load)
  (typing))

(|:| point (-> <real> <real> <point>))
(define-record-type <point>
  (point x y)
  point?
  (x point-x)
  (y point-y))

(|:| distance (-> <point> <point> <real>))
(define (distance p1 p2)
  (sqrt (+ (square (- (point-x pt1) (point-x pt2)))
           (square (- (point-y pt1) (point-y pt2))))))

(|:| A <real>)
(define A 100)

(|:| B <real>)
(define B 200)

(|:| C <point>)
(define C (point A B))

(display A)
(newline)
(display B)
(newline)
(display C)
(newline)

