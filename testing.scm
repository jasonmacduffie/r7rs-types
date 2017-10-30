
;; Example to test type checking

(import
  (scheme base)
  (scheme read)
  (scheme write)
  (scheme load))

(load "./check-types.scm")

(check-global-expression
 '(* 3 4 5))

