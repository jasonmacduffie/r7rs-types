
;; Example to test type checking

(import
  (scheme base)
  (scheme read)
  (scheme write)
  (scheme load))

(load "./check-types.scm")

(display-type
 '(begin
    (define x 3)
    (define y (+ x 3))
    (define z #\c)
    (display y)
    (display x)
    (if z 100 50)))

