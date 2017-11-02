
;; Example to test type checking

(import
  (scheme base)
  (scheme read)
  (scheme write)
  (scheme load))

(load "./check-types.scm")

(display-type
;;  '(begin (car '(1 2 3))))
  '(begin (cdr '(1 2 3 3))))

#| '(begin
    (define l '(1 3 100 -12 3))
    (lambda ()
      (car l))))
    (let loop ((in l) (i 0))
      (if (null? in)
          i
          (loop (cdr in) (+ i 1))))))|#
