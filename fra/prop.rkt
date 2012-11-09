#lang racket/base
(require racket/set
         racket/match
         racket/contract
         racket/list
         racket/dict
         "tuple.rkt"
         "schema.rkt")

(define (prop-print prop port write?)
  (match prop
    [(struct prop:and (lhs rhs))
     (fprintf port "~a ∧ ~a" lhs rhs)]
    [(struct prop:or (lhs rhs))
     (fprintf port "~a ∨ ~a" lhs rhs)]
    [(struct prop:not (lhs))
     (fprintf port "¬ ~a" lhs)]
    [(struct prop:op (op attrs))
     (fprintf port "~a(~a)" (object-name op) attrs)]))

(define-struct prop () #:transparent #:property prop:custom-write prop-print)
(define-struct (prop:and prop) (lhs rhs) #:transparent)
(define-struct (prop:or prop) (lhs rhs) #:transparent)
(define-struct (prop:not prop) (inner) #:transparent)
(define-struct (prop:op prop) (op attrs) #:transparent)

(define compiled-prop/c
  (tuple? . -> . boolean?))

(define prop:true
  (make-prop:op (lambda () #t) empty))

(define (compile-prop psi schema)
  (match psi
    [(struct prop:and (lhs rhs))
     (define lhs-cprop (compile-prop lhs schema))
     (define rhs-cprop (compile-prop rhs schema))
     (lambda (tup)
       (and (lhs-cprop tup) (rhs-cprop tup)))]
    [(struct prop:or (lhs rhs))
     (define lhs-cprop (compile-prop lhs schema))
     (define rhs-cprop (compile-prop rhs schema))
     (lambda (tup)
       (or (lhs-cprop tup) (rhs-cprop tup)))]
    [(struct prop:not (inner))
     (define inner-cprop (compile-prop inner schema))     
     (lambda (tup)
       (not (inner-cprop tup)))]
    [(struct prop:op (op attrs))
     (define refs (map (lambda (a) (schema-ref schema a)) attrs))
     (lambda (tup)
       (apply op (map (lambda (n) (tuple-ref tup n)) refs)))]))

(define prop-schema
  (match-lambda
    [(struct prop:and (lhs rhs))
     (schema-union (prop-schema lhs) (prop-schema rhs))]
    [(struct prop:or (lhs rhs))
     (schema-union (prop-schema lhs) (prop-schema rhs))]
    [(struct prop:not (inner))
     (schema-union (prop-schema inner))]
    [(struct prop:op (op attrs))
     (set->list (list->set attrs))]))

(define (prop-breakup prop Rs Ps)
  (match prop
    [(struct prop:and (lhs rhs))     
     (define-values (Rlhs Plhs RPlhs) (prop-breakup lhs Rs Ps))
     (define-values (Rrhs Prhs RPrhs) (prop-breakup rhs Rs Ps))
     (values (trivial?-and Rlhs Rrhs)
             (trivial?-and Plhs Prhs)
             (trivial?-and RPlhs RPrhs))]
    [(struct prop:or (lhs rhs))     
     (define-values (Rlhs Plhs RPlhs) (prop-breakup lhs Rs Ps))
     (define-values (Rrhs Prhs RPrhs) (prop-breakup rhs Rs Ps))
     (values (trivial?-or Rlhs Rrhs)
             (trivial?-or Plhs Prhs)
             (trivial?-or RPlhs RPrhs))]
    [(struct prop:not (lhs))     
     (define-values (Rlhs Plhs RPlhs) (prop-breakup lhs Rs Ps))
     (values (trivial?-not Rlhs)
             (trivial?-not Plhs)
             (trivial?-not RPlhs))]
    [(struct prop:op (op attrs))
     (cond
       [(schema-subset? attrs Rs)
        (values prop prop:true prop:true)]
       [(schema-subset? attrs Ps)
        (values prop:true prop prop:true)]
       [else
        (values prop:true prop:true prop)])]))

(define (equality-prop renaming [equal? equal?])
  (for/fold ([prop prop:true])
    ([(old new) (in-dict renaming)])
    (trivial?-and prop (make-prop:op equal? (list old new)))))

(define (prop-holds? psi tup) (psi tup))

; XXX More advanced
(define (prop-trivial? p)
  (equal? prop:true p))
(define (trivial?-and lhs rhs)
  (cond
    [(and (prop-trivial? lhs) (prop-trivial? rhs))
     prop:true]
    [(prop-trivial? lhs) rhs]
    [(prop-trivial? rhs) lhs]
    [else (make-prop:and lhs rhs)]))
(define (trivial?-or lhs rhs)
  (cond
    [(and (prop-trivial? lhs) (prop-trivial? rhs))
     prop:true]
    [(prop-trivial? lhs) rhs]
    [(prop-trivial? rhs) lhs]
    [else (make-prop:or lhs rhs)]))
(define (trivial?-not lhs)
  (cond
    [(prop-trivial? lhs) prop:true]
    [else (make-prop:not lhs)]))

(provide/contract
 [prop? (any/c . -> . boolean?)]
 [make-prop:not (prop? . -> . prop?)]
 [struct (prop:and prop) ([lhs prop?] [rhs prop?])]
 [struct (prop:or prop) ([lhs prop?] [rhs prop?])]
 ; XXX Better
 [make-prop:op (procedure? (listof symbol?) . -> . prop?)]
 [prop:true prop?]
 [compiled-prop/c contract?]
 [prop-schema (prop? . -> . schema/c)]
 [prop-breakup (prop? schema/c schema/c . -> . (values prop? prop? prop?))]
 [compile-prop (prop? schema/c . -> . compiled-prop/c)]
 [prop-holds? (compiled-prop/c tuple? . -> . boolean?)]
 [prop-trivial? (prop? . -> . boolean?)]
 [equality-prop (((hash/c symbol? symbol?)) ((any/c any/c . -> . boolean?)) . ->* . prop?)])

(define-syntax Proposition
  (syntax-rules (not and or)
    [(_ (not p))
     (make-prop:not (Proposition p))]
    [(_ (and p1 p2))
     (make-prop:and (Proposition p1) (Proposition p2))]
    [(_ (or p1 p2))
     (make-prop:or (Proposition p1) (Proposition p2))]
    [(_ (op attr ...))
     (make-prop:op op (list 'attr ...))]))

(provide Proposition)
