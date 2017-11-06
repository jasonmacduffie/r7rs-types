R7RS Types
==========
Optional static typing and static analysis tool for R7RS Scheme

Status
------
Still in the early stages. Syntax for type annotation is still not
determined.

Purpose
-------
Optional types, gradual types, and type annotations are increasingly
common features of dynamically typed languages. Examples of languages
with varying degrees of typing are Typescript, Python, PHP, Common Lisp,
Clojure, and Typed Racket.

The purposes of type annotations are several:

- To provide documentation for humans
- To allow compiler optimizations
- To detect type errors before runtime
- To improve tooling support

As dynamically typed languages are used for increasingly large projects,
the need for some type information to improve maintainability has been
noticed. By providing a basic framework by which variables in Scheme can
be annotated, there may be greater potential for Scheme usage in industrial
settings.

Usage
-----
The syntax forms exported from this library are meaningless,
and are intended to document the types of variables and
procedures.

For portable code, it is suggested to use pipes for the symbol
|:|, as colons have special meanings in some Scheme
implementations.

Note that only forms using "define" and "define-record-type"
are supported, so anonymous functions may be assumed to accept
and return types <any>.

Types being supported:
<any> <vector> <list> <number> <real> <complex> <rational>
<boolean> ... (WIP)

Procedures have their own type forms:
(-> <input type> ... <output type>)

EXAMPLE USAGE, based on a Typed Racket example:

    (|:| point (-> <real> <real> <point>))
    (define-record-type <point>
      (point x y)
      point?
      (x point-x)
      (y point-y))

    (|:| distance (-> <point> <point> <real>))
    (define (distance p1 p2)
      (sqrt (+ (square (- (point-x pt1) (point-x pt2)))
               (square (- (point-y pt1) (point-y pt2))))))

    (|:| A <real>)
    (define A 100)

    (|:| B <real>)
    (define B 200)

    (|:| C <point>)
    (define C (point A B))

    etcetera...

License
-------
GPLv3 or any later version. See LICENSE for details.
