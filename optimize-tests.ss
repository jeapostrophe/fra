#lang scheme
(require "optimize.ss"
         "prop.ss"
         "query.ss")

(define (even? x) #f)
(define (odd? x) #f)
(current-database-schema
 (lambda (rel-id)
   (case rel-id
     [(C1) 'A]
     [(C2) 'B]
     [else
      empty])))

(require tests/eli-tester)
(test
 ; Cross
 (push-down/once
  (make-q:selection (Proposition (and (even? 'A) (odd? 'B)))
                    (make-q:product (query-relation 'C1) (query-relation 'C2))))
 =>
 (make-q:selection prop:true
                   (make-q:product
                    (make-q:selection (Proposition (even? 'A)) (query-relation 'C1))
                    (make-q:selection (Proposition (odd? 'B)) (query-relation 'C2))))
 
 ; Triviality
 (simplify/once
  (make-q:selection prop:true (query-relation 'R)))
 =>
 (query-relation 'R)
  
 ; Selection idempotency
 (simplify/once
  (make-q:selection (Proposition (even? a))
                    (make-q:selection (Proposition (even? a)) (query-relation 'R))))
 =>
 (make-q:selection (Proposition (even? a)) (query-relation 'R))
 
 (simplify/once
  (make-q:selection (Proposition (even? a))
                    (make-q:selection (Proposition (odd? a)) (query-relation 'R))))
 =>
 (make-q:selection (Proposition (even? a))
                    (make-q:selection (Proposition (odd? a)) (query-relation 'R)))
 
 ; Complex Conditions
 (pull-up/once
  (make-q:union (make-q:selection (Proposition (even? a)) (query-relation 'R))
                (make-q:selection (Proposition (odd? a)) (query-relation 'R))))
 =>
 (make-q:selection (Proposition (or (even? a) (odd? a))) (query-relation 'R))
 
 (pull-up/once
  (make-q:selection (Proposition (even? a))
                    (make-q:selection (Proposition (odd? a))
                                      (query-relation 'R))))
 =>
 (make-q:selection (Proposition (and (even? a) (odd? a))) (query-relation 'R))
 
 ; Distribute selection
 (pull-up/once
  (make-q:difference (make-q:selection (Proposition (even? a)) (query-relation 'R))
                     (make-q:selection (Proposition (even? a)) (query-relation 'P))))
 =>
 (make-q:selection (Proposition (even? a)) 
                   (make-q:difference (query-relation 'R)
                                      (query-relation 'P)))
 (pull-up/once
  (make-q:difference (make-q:selection (Proposition (even? a)) (query-relation 'R)) (query-relation 'P)))
 =>
 (make-q:selection (Proposition (even? a)) 
                   (make-q:difference (query-relation 'R)
                                      (query-relation 'P)))
 (pull-up/once
  (make-q:union (make-q:selection (Proposition (even? a)) (query-relation 'R))
                (make-q:selection (Proposition (even? a)) (query-relation 'P))))
 =>
 (make-q:selection (Proposition (even? a)) 
                   (make-q:union (query-relation 'R)
                                 (query-relation 'P)))
 (pull-up/once
  (make-q:intersection (make-q:selection (Proposition (even? a)) (query-relation 'R))
                       (make-q:selection (Proposition (even? a)) (query-relation 'P))))
 =>
 (make-q:selection (Proposition (even? a)) 
                   (make-q:intersection (query-relation 'R)
                                        (query-relation 'P)))
 (pull-up/once
  (make-q:intersection (make-q:selection (Proposition (even? a)) (query-relation 'R))
                       (query-relation 'P)))
 =>
 (make-q:selection (Proposition (even? a)) 
                   (make-q:intersection (query-relation 'R)
                                        (query-relation 'P)))
 (pull-up/once
  (make-q:intersection (query-relation 'R)
                       (make-q:selection (Proposition (even? a)) (query-relation 'P))))
 =>
 (make-q:selection (Proposition (even? a)) 
                   (make-q:intersection (query-relation 'R)
                                        (query-relation 'P)))
 
 
 ; Lift selection
 (pull-up/once
  (make-q:projection '(a) (make-q:selection (Proposition (even? a)) (query-relation 'R))))
 =>
 (make-q:selection (Proposition (even? a)) (make-q:projection '(a) (query-relation 'R)))
 
 (pull-up/once
  (make-q:projection '(a) (make-q:selection (Proposition (even? b)) (query-relation 'R))))
 =>
 (make-q:projection '(a) (make-q:selection (Proposition (even? b)) (query-relation 'R)))
 
 ; Merge Projections
 (simplify/once
  (make-q:projection '(a) (make-q:projection '(a b) (query-relation 'R))))
 =>
 (make-q:projection '(a) (query-relation 'R))
 
 ; Distribute over set opps
 (pull-up/once
  (make-q:union (make-q:projection '(a) (query-relation 'R)) (make-q:projection '(a) (query-relation 'S))))
 => (make-q:projection '(a) (make-q:union (query-relation 'R) (query-relation 'S)))
 (pull-up/once
  (make-q:union (make-q:projection '(a) (query-relation 'R)) (make-q:projection '(b) (query-relation 'S))))
 => (make-q:union (make-q:projection '(a) (query-relation 'R)) (make-q:projection '(b) (query-relation 'S)))
 (pull-up/once
  (make-q:difference (make-q:projection '(a) (query-relation 'R)) (make-q:projection '(a) (query-relation 'S))))
 => (make-q:projection '(a) (make-q:difference (query-relation 'R) (query-relation 'S)))
 (pull-up/once
  (make-q:difference (make-q:projection '(a) (query-relation 'R)) (make-q:projection '(b) (query-relation 'S))))
 => (make-q:difference (make-q:projection '(a) (query-relation 'R)) (make-q:projection '(b) (query-relation 'S)))
 (pull-up/once
  (make-q:intersection (make-q:projection '(a) (query-relation 'R)) (make-q:projection '(a) (query-relation 'S))))
 => (make-q:projection '(a) (make-q:intersection (query-relation 'R) (query-relation 'S)))
 (pull-up/once
  (make-q:intersection (make-q:projection '(a) (query-relation 'R)) (make-q:projection '(b) (query-relation 'S))))
 => (make-q:intersection (make-q:projection '(a) (query-relation 'R)) (make-q:projection '(b) (query-relation 'S)))
 
 ; Merge Renamings
 (simplify/once
  (make-q:rename* (make-immutable-hasheq '((b . c))) (make-q:rename* (make-immutable-hasheq '((a . b))) (query-relation 'R))))
 =>
 (make-q:rename* (make-immutable-hasheq '((a . c))) (query-relation 'R))
 
 (simplify/once
  (make-q:rename* (make-immutable-hasheq '((d . c))) (make-q:rename* (make-immutable-hasheq '((a . b))) (query-relation 'R))))
 =>
 (make-q:rename* (make-immutable-hasheq '((a . b) (d . c))) (query-relation 'R))
 
 ; Distribute rename over union
 (pull-up/once
  (make-q:union (make-q:rename* (make-immutable-hasheq '((a . b))) (query-relation 'R))
                (make-q:rename* (make-immutable-hasheq '((a . b))) (query-relation 'S))))
 =>
 (make-q:rename* (make-immutable-hasheq '((a . b))) 
                 (make-q:union (query-relation 'R)
                               (query-relation 'S)))
 
 (pull-up/once
  (make-q:union (make-q:rename* (make-immutable-hasheq '((a . b))) (query-relation 'R))
                (make-q:rename* (make-immutable-hasheq '((a . d))) (query-relation 'S))))
 =>
 (make-q:union (make-q:rename* (make-immutable-hasheq '((a . b))) (query-relation 'R))
               (make-q:rename* (make-immutable-hasheq '((a . d))) (query-relation 'S)))
 
 ; Distribute rename over difference
 (pull-up/once
  (make-q:difference (make-q:rename* (make-immutable-hasheq '((a . b))) (query-relation 'R))
                     (make-q:rename* (make-immutable-hasheq '((a . b))) (query-relation 'S))))
 =>
 (make-q:rename* (make-immutable-hasheq '((a . b))) 
                 (make-q:difference (query-relation 'R)
                                    (query-relation 'S)))
 
 (pull-up/once
  (make-q:difference (make-q:rename* (make-immutable-hasheq '((a . b))) (query-relation 'R))
                     (make-q:rename* (make-immutable-hasheq '((a . d))) (query-relation 'S))))
 =>
 (make-q:difference (make-q:rename* (make-immutable-hasheq '((a . b))) (query-relation 'R))
                    (make-q:rename* (make-immutable-hasheq '((a . d))) (query-relation 'S)))
 
 ; Distribute rename over intersection
 (pull-up/once
  (make-q:intersection (make-q:rename* (make-immutable-hasheq '((a . b))) (query-relation 'R))
                       (make-q:rename* (make-immutable-hasheq '((a . b))) (query-relation 'S))))
 =>
 (make-q:rename* (make-immutable-hasheq '((a . b))) 
                 (make-q:intersection (query-relation 'R)
                                      (query-relation 'S)))
 
 (pull-up/once
  (make-q:intersection (make-q:rename* (make-immutable-hasheq '((a . b))) (query-relation 'R))
                       (make-q:rename* (make-immutable-hasheq '((a . d))) (query-relation 'S))))
 =>
 (make-q:intersection (make-q:rename* (make-immutable-hasheq '((a . b))) (query-relation 'R))
                      (make-q:rename* (make-immutable-hasheq '((a . d))) (query-relation 'S)))
 
 )