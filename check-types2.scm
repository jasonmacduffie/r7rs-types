

;; Second attempt

(import
  (scheme base)
  (scheme read)
  (scheme write)
  (srfi 1))

(define (any-type args)
  (cond
   ((eq? args 'is-any?)
    #t)
   ((eq? args 'type-predicate)
    'is-any?)
   (else
    #t)))

(define (none-type args)
  (cond
   ((eq? args 'is-none?)
    #t)
   ((eq? args 'type-predicate)
    'is-none?)
   (else
    #f)))

(define (simple-union-of t1 t2)
  (lambda (arg)
    (cond
     ((eq? arg 'set-car-of-second-to!)
      ;; Specifically for lists.
      (lambda (self)
        ((t2 'set-car-to!) self)))
     (else
      (or (t1 arg)
          (t2 arg))))))

(define (union-of . l)
  (reduce simple-union-of (car l) (cdr l)))

(define (tuple-of . types)
  (lambda (arg)
    (cond
     ((eq? arg 'tuple-ref)
      (lambda (i)
        (list-ref types i)))
     (else
      #f))))

(define (null-type arg)
  (cond
   ((eq? arg 'is-null?)
    #t)
   (else
    #f)))

(define (list-of t)
  (define result (simple-union-of null-type (pair-of t 0)))
  ((result 'set-car-of-second-to!) result)
  (lambda (arg)
    (cond
     ((eq? arg 'is-list?)
      #t)
     ((eq? arg 'type)
      t)
     ((eq? arg 'type-predicate)
      'is-list?)
     ((eq? arg 'parametric?)
      #t)
     ((eq? arg 'type-predicate)
      'is-list?)
     (else
      (result arg)))))

(define (pair-of t1 t2)
  (lambda (arg)
    (cond
     ((eq? arg 'is-pair?)
      #t)
     ((eq? arg 'car-type)
      t1)
     ((eq? arg 'cdr-type)
      t2)
     ((eq? arg 'parametric?)
      #t)
     ((eq? arg 'type-predicate)
      'is-pair?)
     ((eq? arg 'set-car-to!)
      ;; Specifically for lists.
      (lambda (value)
        (set! t2 value)))
     (else
      #f))))

(define (vector-of t)
  (lambda (arg)
    (cond
     ((eq? arg 'is-vector?)
      #t)
     ((eq? arg 'type)
      t)
     ((eq? arg 'parametric?)
      #t)
     ((eq? arg 'type-predicate)
      'is-vector?)
     (else
      #f))))

(define (boolean-type arg)
  (cond
   ((eq? arg 'is-boolean?)
    #t)
   ((eq? arg 'is-true?)
    #t)
   ((eq? arg 'is-false?)
    #t)
   ((eq? arg 'type-predicate)
    'is-boolean?)
   (else
    #f)))

(define (false-type arg)
  ;; boolean-type, restricted to false
  (cond
   ((eq? arg 'is-false?)
    #t)
   ((eq? arg 'type-predicate)
    'is-false?)
   (else
    #f)))

(define (true-type arg)
  ;; boolean-type, restricted to true
  (cond
   ((eq? arg 'is-true?)
    #t)
   ((eq? arg 'type-predicate)
    'is-true?)
   (else
    #f)))

(define (maybe-of t)
  ;; failure in Scheme is usually represented as false
  ;; assoc, member, string->number, etc.
  ;; maybe-of is a union of a type and false
  (define result (simple-union-of false-type t))
  (lambda (arg)
    (cond
     ((eq? arg 'is-maybe?)
      #t)
     ((eq? arg 'parametric?)
      #t)
     ((eq? arg 'type-predicate)
      'is-maybe?)
     (else
      (result arg)))))

(define (bytevector-type arg)
  (cond
   ((eq? arg 'is-bytevector?)
    #t)
   ((eq? arg 'type-predicate)
    'is-bytevector?)
   (else
    #f)))

(define (eof-object-type arg)
  (cond
   ((eq? arg 'is-eof-object?)
    #t)
   ((eq? arg 'type-predicate)
    'is-eof-object?)
   (else
    #f)))

(define (number-type arg)
  (cond
   ((eq? arg 'is-number?)
    #t)
   ((eq? arg 'type-predicate)
    'is-number?)
   ((eq? arg 'is-complex?)
    #t)
   ((eq? arg 'is-real?)
    #t)
   ((eq? arg 'is-rational?)
    #t)
   ((eq? arg 'is-integer?)
    #t)
   (else
    #f)))

(define (port-type arg)
  (cond
   ((eq? arg 'is-port?)
    #t)
   ((eq? arg 'type-predicate)
    'is-port?)
   (else
    #f)))

(define (string-type arg)
  (cond
   ((eq? arg 'is-string?)
    #t)
   ((eq? arg 'type-predicate)
    'is-string?)
   (else
    #f)))

(define (symbol-type arg)
  (cond
   ((eq? arg 'is-symbol?)
    #t)
   ((eq? arg 'type-predicate)
    'is-symbol?)
   (else
    #f)))

;;; Predicates
(define (any-type? t)
  (t 'any-type?))

(define (none-type? t)
  (t 'none-type?))

(define (null-type? t)
  (t 'is-null?))

(define (list-type? t)
  (t 'is-list?))

(define (pair-type? t)
  (t 'is-pair?))

(define (vector-type? t)
  (t 'is-vector?))

(define (boolean-type? t)
  (t 'is-boolean?))

(define (maybe-type? t)
  (t 'is-maybe?))

(define (bytevector-type? t)
  (t 'is-bytevector?))

(define (eof-object-type? t)
  (t 'is-eof-object?))

(define (number-type? t)
  (t 'is-number?))

(define (port-type? t)
  (t 'is-port?))

(define (string-type? t)
  (t 'is-string?))

(define (symbol-type? t)
  (t 'is-symbol?))

(define (procedure-type? t)
  (t 'is-procedure?))

(define (parametric? t)
  (t 'parametric?))

;;; De-parameterization
(define (type-car t)
  (t 'car-type))

(define (type-cdr t)
  (t 'cdr-type))

(define (type-vector t)
  (t 'type))

(define (type-list t)
  (t 'car-type))

;; Record types
(define (tuple-ref t i)
  ((t 'tuple-ref) i))

#|
;;(list-of <number>)
;;(pair-of <symbol> <any>)
;;(vector-of <number>)
;;(maybe-of (pair-of <symbol> <any>))
;;(-> (<number> <number> <string>))
|#

(define (counts-as-a type-of-expr type-of-var)
  ;; TODO: check parametric types
  (if (or (any-type? type-of-expr)
          (any-type? type-of-var))
      #t
      (if (parametric? type-of-var)
          (cond
           ((maybe-type? type-of-var)
            (type-of-var (type-of-expr 'type-predicate)))
           ((list-type? type-of-var)
            (and (type-of-var (type-of-expr 'type-predicate))
                 (counts-as-a (type-of-expr 'type)
                              (type-of-var 'type))))
           ((pair-type? type-of-var)
            (and (type-of-var (type-of-expr 'type-predicate))
                 (counts-as-a (type-of-expr 'car-type)
                              (type-of-var 'car-type))
                 (counts-as-a (type-of-expr 'cdr-type)
                              (type-of-var 'cdr-type))))
           (else
            (and (type-of-var (type-of-expr 'type-predicate))
                 (counts-as-a (type-of-expr 'type)
                              (type-of-var 'type)))))
          (type-of-var (type-of-expr 'type-predicate)))))

(define (read-type expr)
  ;; Read a type annotation
  (if (pair? expr)
      ;; If a pair, then it is a parameterized or a procedure
      (display "next 4")
      (cond
       ((eq? expr '<boolean>)
        boolean-type)
       ((eq? expr '<bytevector>)
        bytevector-type)
       ((eq? expr '<char>)
        char-type)
       ((eq? expr '<eof-object>)
        eof-object-type)
       ((eq? expr '<null>)
        null-type)
       ((eq? expr '<number>)
        number-type)
       ((eq? expr '<port>)
        port-type)
       ((eq? expr '<string>)
        string-type)
       ((eq? expr '<symbol>)
        symbol-type)
       ((eq? expr '<any>)
        any-type)
       ((eq? expr '<none>)
        none-type)
       (else
        ;; Otherwise, display a warning and return any-type
        (display "WARNING: Unknown type annotation: ")
        (display expr)
        (newline)
        any-type))))
       
(define (check-expressions exprs context)
  (let loop ((in exprs)
             (env context))
    (if (null? in)
        #t
        (let ((next (car in)))
          (cond
           ((pair? next)
            (let ((form-type (car next)))
              (cond
               ((eq? form-type '|:|)
                (loop (cdr in)
                      (cons
                       (cons (list-ref next 1)
                             (read-type (list-ref next 2)))
                       env)))
               ((eq? form-type 'define)
                (if (eq? (list-ref next 1) (caar env))
                    (begin
                      (valid-type (list-ref next 2) (cdar env))
                      (loop (cdr in) env))
                    (loop (cdr in)
                          (cons
                           (cons (list-ref next 1)
                                 any-type)
                           env))))
               (else
                (display "next 2")))))
           (else
            (display "next 3")
            (loop (cdr in) env)))))))

