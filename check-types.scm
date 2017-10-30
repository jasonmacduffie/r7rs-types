
;; First attempt at a static type checker

(import
  (scheme base)
  (scheme read)
  (scheme write))

(define-record-type <type>
  (make-type parameters representation union-of)
  type?
  (parameters type-params)
  (representation type-repr)
  (union-of type-union))

(define (warn-any)
  (display "Any type was found")
  (newline))

(define (type=? a b)
  (unless (and (type? a) (type? b))
    (error "type=?" "Both arguments must be types" a b))
  (cond
   ((or (eq? a any-type)
        (eq? b any-type))
    (warn-any)
    #t)
   ((or (union-type? a) (union-type? b))
    #t) ;; TODO: implement union type checking
   ((parameterized-type? a)
    (if (parameterized-type? b)
        (if (eq? (car (type-params a))
                 (car (type-params b)))
            (let loop ((a-in (cdr (type-params a)))
                       (b-in (cdr (type-params b))))
              (if (null? a-in)
                  (null? b-in)
                  (if (type=? (car a-in) (car b-in))
                      (loop (cdr a-in) (cdr b-in))
                      #f)))
            #f)
        #f))
   (else
    (eq? a b))))

(define (union-type? t)
  (if (type-union t) #t #f))

(define (parameterized-type? t)
  (if (type-params t) #t #f))

(define any-type (make-type "#<any>" #f #f))
(define number-type (make-type "#<number>" #f #f))
(define char-type (make-type "#<char>" #f #f))
(define string-type (make-type "#<string>" #f #f))
(define null-type (make-type "#<null>" #f #f))
(define (vector-of t)
  (make-type (string-append "#<vector-of " (type-repr t) ">") (list vector-of t) #f))
(define (pair-of a b)
  (make-type (string-append "#<pair-of (" (type-repr a) ")x(" (type-repr b) ")>" (list pair-of a b) #f)))
(define (list-of t)
  (pair-of t any-type))
;;  (make-type (string-append "#<list-of " (type-repr t) 
;;  (make-type (list t) (string-append "#<list-of " (type-repr t) ">")



;;(define (check-expression

