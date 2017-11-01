
;; Example to test type checking

(import
  (scheme base)
  (scheme read)
  (scheme write)
  (scheme load))

(load "./check-types.scm")

(display-type
 '(begin
    (define l '(1 3 100 -12 3))
    (let loop ((in l) (i 0))
      (if (null? in)
          i
          (loop (cdr in) (+ i 1))))))
