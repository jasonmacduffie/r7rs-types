
;; Example to test type checking

(import
  (scheme base)
  (scheme read)
  (scheme write)
  (scheme load))

(load "./check-types.scm")

(display-type
 '(begin
    (if 100 '(1 3) '(1 3))))
