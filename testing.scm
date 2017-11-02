
;; Example to test type checking

(import
  (scheme base)
  (scheme read)
  (scheme write)
  (scheme load))

(load "./check-types.scm")

(display-type
  '(begin
     (define p '(100 . 200))
     (car p)))
#|



 '(begin
    (define l '(1 3 100 -12 3))
    (lambda ()
      (car l))))
    (let loop ((in l) (i 0))
      (if (null? in)
          i
          (loop (cdr in) (+ i 1))))))
|#
