
;; First attempt at a static type checker

(import
  (scheme base)
  (scheme read)
  (scheme write)
  (scheme cxr))

(define-record-type <type>
  (make-type representation parameters)
  type?
  (representation type-repr)
  (parameters type-params))

(define fundamental-syntax
  ;; I assume you do not override these fundamental syntax forms, and
  ;; that these forms are indeed imported. If you are using a language
  ;; besides (scheme base) this checker will not work.
  ;;
  ;; All other syntax forms must be pre-expanded in order for type checking
  ;; to work.
  '(define begin lambda if case cond or and let))

(define (syntax-form? l)
  (if (memq (car l) fundamental-syntax)
      #t
      #f))

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

(define (parameterized-type? t)
  (if (type-params t) #t #f))

(define any-type (make-type "#<any>" #f))
(define undefined-type (make-type "#<undefined>" #f))
(define number-type (make-type "#<number>" #f))
(define boolean-type (make-type "#<boolean>" #f))
(define char-type (make-type "#<char>" #f))
(define string-type (make-type "#<string>" #f))
(define null-type (make-type "#<null>" #f))
(define (vector-of t)
  (make-type (string-append "#<vector-of " (type-repr t) ">") (list vector-of t)))
(define (pair-of a b)
  (make-type (string-append "#<pair-of (" (type-repr a) ")x(" (type-repr b) ")>") (list pair-of a b)))
(define (list-of t)
  (pair-of t any-type))
(define (procedure-of input-types output-type variadic?)
  (make-type (string-append "#<procedure: ("
                            (apply string-append
                             (map (lambda (s)
                                    (string-append (type-repr s)
                                                   " "))
                                  (if variadic?
                                      (reverse (cdr (reverse input-types)))
                                      input-types)))
                            (if variadic?
                                (string-append " . ("
                                               (type-repr (car (reverse input-types)))
                                               ")")
                                "")
                            ") -> "
                            (type-repr output-type)
                            ">")
             (list procedure-of input-types output-type variadic?)))

(define (procedure-type? t)
  (and (type? t)
       (parameterized-type? t)
       (eq? (car (type-params t)) procedure-of)))

(define global-context
  ;; This has the type signature of built-in functions
  `((* . ,(procedure-of (list number-type number-type)
                       number-type
                       #t))
    (/ . ,(procedure-of (list number-type number-type)
                        number-type
                        #t))
    (+ . ,(procedure-of (list number-type)
                        number-type
                        #t))
    (- . ,(procedure-of (list number-type)
                        number-type
                        #t))))

(define (check-expression expr context)
  ;; Returns the type of expr. If a type error is found,
  ;; raise an error.
  (cond
   ((pair? expr)
    (if (list? expr)
        (check-list expr context)
        (error "check-expression" "Improper list cannot be evaluated")))
   ((null? expr)
    (error "check-expression" "Null cannot be evaluated"))
   ((number? expr)
    number-type)
   ((boolean? expr)
    boolean-type)
   ((char? expr)
    char-type)
   ((string? expr)
    string-type)
   ((vector? expr)
    (check-vector expr))
   ((symbol? expr)
    (let ((result (assq expr context)))
      (if result
          (cdr result)
          (error "check-expression" "No context for symbol" expr))))
   (else
    (error "check-expression" "Not implemented for " expr))))

(define (check-list expr context)
  (if (syntax-form? expr)
      (check-syntax-form expr context)
      (let ((applyer (check-expression (car expr) context))
            (applied (map (lambda (e) (check-expression e context))
                          (cdr expr))))
        (if (procedure-type? applyer) ;; TODO: check arguments
            (if (arguments-match? applyer applied)
                (list-ref (type-params applyer) 2)
                (error "check-list" "Argument type mismatch"))
            (error "check-list" "Non-procedure application" applyer)))))

(define (check-syntax-form expr context)
  (cond
   ((eq? (car expr) 'begin)
    (step-through (cdr expr) context))
   (else (error "check-syntax-form" "Not yet implemented" (car expr)))))

(define (step-through exprs context)
  (if (null? exprs)
      undefined-type
      (if (null? (cdr exprs))
          (check-expression (car exprs) context)
          (let ((next-expression (car exprs)))
            (step-through (cdr exprs)
                          (if (and (pair? next-expression)
                                   (eq? 'define (car next-expression)))
                              (cond
                               ((symbol? (cadr next-expression))
                                (cons (cons (cadr next-expression) (check-expression (list-ref next-expression 2) context))
                                      context))
                               ((pair? (cadr next-expression))
                                ;; TODO: work on syntactic sugar for lambda
                                (error "step-through" "Syntactic sugar for procedure definitions not yet implemented" next-expression))
                               (else
                                (error "step-through" "Invalid define syntax" next-expression)))
                              context))))))

(define (arguments-match? proc-type args)
  (let ((variadic? (list-ref (type-params proc-type) 3)))
    (let ((proc-size (length (list-ref (type-params proc-type) 1)))
          (args-size (length args)))
      (when (and variadic?
                 (> (- proc-size 1) args-size))
        (error "arguments-match?" "Expected at least "
               (- proc-size 1) " arguments, got " args-size))
      (unless (or variadic? (= proc-size args-size))
        (error "arguments-match?" "Expected " proc-size
               "arguments, got " args-size)))
    (let loop ((proc-in (list-ref (type-params proc-type) 1))
               (args-in args))
      (let ((checking-variadic? (and variadic? (= (length proc-in) 1))))
        (if (null? args-in)
            (or checking-variadic? (null? proc-in))
            (if (null? proc-in)
                #f
                (if (type=? (car proc-in) (car args-in))
                    (loop (if checking-variadic?
                              proc-in
                              (cdr proc-in))
                          (cdr args-in))
                      (error "Expected "
                        (type-repr (car proc-in))
                        ", but got "
                        (type-repr (car args-in))))))))))

(define (check-global-expression expr)
  (check-expression expr global-context))

(define (display-type expr)
  ;; Just for testing purposes
  (display (type-repr (check-global-expression expr)))
  (newline))

