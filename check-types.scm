
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
  '(define begin quote lambda if case cond or and let import))

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
   ((or (eq? a garbage-type)
        (eq? b garbage-type))
    #f)
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

(define garbage-type (make-type "#<garbage>" #f)) ;; opposite of any
(define any-type (make-type "#<any>" #f))
(define undefined-type (make-type "#<undefined>" #f))
(define number-type (make-type "#<number>" #f))
(define boolean-type (make-type "#<boolean>" #f))
(define char-type (make-type "#<char>" #f))
(define string-type (make-type "#<string>" #f))
(define (vector-of t)
  (make-type (string-append "#<vector-of " (type-repr t) ">") (list vector-of t)))
(define (pair-of a b)
  (make-type (string-append "#<pair-of (" (type-repr a) ")x(" (type-repr b) ")>") (list pair-of a b)))
(define (list-of t)
  ;; Note that the list type is disjoint from the pair type.
  ;; This is necessary because we cannot make a union of null and pair.
  (make-type (string-append "#<list-of " (type-repr t) ">") (list list-of t)))
(define (procedure-of input-types output-type-fetcher variadic?)
  (make-type (string-append "#<procedure: ("
                            (apply string-append
                             (map (lambda (s)
                                    (string-append (s 'string-repr)
                                                   " "))
                                  (if variadic?
                                      (reverse (cdr (reverse input-types)))
                                      input-types)))
                            (if variadic?
                                (string-append " . ("
                                               ((car (reverse input-types)) 'string-repr)
                                               ")")
                                "")
                            ") -> "
                            (output-type-fetcher 'string-repr)
                            ">")
             (list procedure-of input-types output-type-fetcher variadic?)))
(define (maybe-of t)
  ;; maybe-of can be of type t, or #f
  (make-type (string-append "#<maybe-of " (type-repr t) ">") (list maybe-of t)))

(define (simple-input-type t)
  (lambda (arg)
    (cond
     ((eq? arg 'get-type-func)
      (lambda ()
        t))
     ((eq? arg 'string-repr)
      (type-repr t))
     ((eq? arg 'parametric?)
      #f)
     (else
      (error "simple-input-type" "Not understood" arg)))))

(define (parametric-input-type maker)
  (lambda (arg)
    (cond
     ((eq? arg 'get-type-func)
      (lambda params
        (apply maker params)))
     ((eq? arg 'string-repr)
      "PARAMETRIC INPUT")
     ((eq? arg 'parametric?)
      #t)
     ((eq? arg 'maker)
      maker)
     (else
      (error "parametric-input-type" "Not understood" arg)))))

(define (simple-output-type t)
  (lambda (proc)
    (cond
     ((eq? proc 'string-repr)
      (type-repr t))
     ((eq? proc 'parametric?)
      #f)
     (else t))))

(define (parametric-output-type fetcher)
  (lambda args
    (cond
     ((eq? (car args) 'string-repr)
      "PARAMETRIC OUTPUT")
     ((eq? (car args) 'parametric?)
      #t)
     (else
      (fetcher (list-ref args 1))))))

(define (procedure-type? t)
  (and (type? t)
       (parameterized-type? t)
       (eq? (car (type-params t)) procedure-of)))

(define global-context
  ;; This has the type signature of built-in functions
  ;; TODO: fix car and cdr
  `((car . ,(procedure-of (list (parametric-input-type pair-of))
                          (parametric-output-type (lambda (intypes)
                                                    (caddr (type-params (car intypes)))))
                          #f))
    (cdr . ,(procedure-of (list (parametric-input-type pair-of))
                          (simple-output-type any-type)
                          #f))
    (* . ,(procedure-of (list (simple-input-type number-type))
                        (simple-output-type number-type)
                        #t))
    (/ . ,(procedure-of (list (simple-input-type number-type) (simple-input-type number-type))
                        (simple-output-type number-type)
                        #t))
    (+ . ,(procedure-of (list (simple-input-type number-type))
                        (simple-output-type number-type)
                        #t))
    (- . ,(procedure-of (list (simple-input-type number-type) (simple-input-type number-type))
                        (simple-output-type number-type)
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
                (get-proc-output applyer applied)
                (error "check-list" "Argument type mismatch"))
            (error "check-list" "Non-procedure application" applyer)))))

(define (get-proc-output applyer applied)
  ((list-ref (type-params applyer) 2) 'bind-input-params applied))

(define (check-syntax-form expr context)
  (cond
   ((eq? (car expr) 'begin)
    (step-through (cdr expr) context))
   ((eq? (car expr) 'lambda)
    ;; TODO: lambda
    (procedure-of (list any-type)
                  (step-through (cddr expr) (append (map (lambda (v) (cons v any-type))
                                                         (cadr expr))
                                                    context))
                  #t))
   ((eq? (car expr) 'let)
    (if (list? (cadr expr))
        (check-expression (apply list (apply list 'lambda (map car (cadr expr)) (cddr expr)) (map cadr (cadr expr)))
                          context)
        (check-expression (list (list 'lambda '()
                                      (list 'define (cadr expr)
                                            (list 'lambda (map car (caddr expr))
                                                  (cadddr expr)))
                                      (cons (cadr expr)
                                            (map cadr (caddr expr)))))
                          context)))
   ((eq? (car expr) 'quote)
    (if (= (length expr) 2)
        (check-quoted-expression (cadr expr))
        (error "check-syntax-form" "malformed quote" expr)))
   ((eq? (car expr) 'if)
    (cond
     ((= (length expr) 3)
      ;; If expressions can be anything without an else clause.
      any-type)
     ((= (length expr) 4)
      (let ((expr1-type (check-expression (list-ref expr 2) context))
            (expr2-type (check-expression (list-ref expr 3) context)))
        (if (type=? expr1-type expr2-type)
            expr1-type
            (if (not (list-ref expr 3))
                ;; If return non-boolean or false, it is a "maybe" type
                (maybe-of expr1-type)
                (if (not (list-ref expr 2))
                    (maybe-of expr2-type)
                    any-type)))))
     (else (error "check-syntax-form" "malformed if" expr))))        
   (else (error "check-syntax-form" "Not yet implemented" (car expr)))))

(define (check-quoted-expression expr)
  (cond
   ((list? expr)
    (cond
     ((null? expr)
      (list-of any-type))
     ((null? (cdr expr))
      (list-of (check-quoted-expression (car expr))))
     (else
      (let loop ((in (cddr expr))
                 (prev-type (check-quoted-expression (cadr expr))))
        (if (null? in)
            (list-of prev-type)
            (if (type=? prev-type (check-quoted-expression (car in)))
                (loop (cdr in) prev-type)
                (list-of any-type)))))))
   ((pair? expr)
    (pair-of (check-quoted-expression (car expr))
             (check-quoted-expression (cdr expr))))
   ((symbol? expr)
    symbol-type)
   (else
    (check-expression expr '()))))

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
                                (cons (cons (cadr next-expression) (check-expression (list-ref next-expression 2)
                                                                                     (cons (cons (cadr next-expression)
                                                                                                 any-type)
                                                                                           context)))
                                      ;; TODO: recursive references
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
                (if (type=? (if ((car proc-in) 'parametric?)
                                (if (proc-params-compatible? (car proc-in) (car args-in))
                                    any-type
                                    garbage-type)
                                ((car proc-in)))
                            (car args-in))
                    (loop (if checking-variadic?
                              proc-in
                              (cdr proc-in))
                          (cdr args-in))
                      (error "Expected "
                        ((car proc-in) 'string-repr)
                        ", but got "
                        (type-repr (car args-in))))))))))

(define (proc-params-compatible? proc-in-type t)
  (and
   (parameterized-type? t)
   (eq? (proc-in-type 'maker) (car (type-params t)))))

(define (check-global-expression expr)
  (check-expression expr global-context))

(define (display-type expr)
  ;; Just for testing purposes
  (display (type-repr (check-global-expression expr)))
  (newline))

