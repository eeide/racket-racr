; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger
; Ported to Racket by: Eric Eide

#lang racket

(require "../../racr/core.rkt")
(provide
         specify-analyses pn
         :AtomicPetrinet :Place :Token :Transition :Arc
         ->Place* ->Transition* ->Token* ->In ->Out
         ->name ->value ->place ->consumers ->* <-
         =places =transitions =in-arcs =out-arcs
         =p-lookup =t-lookup =in-lookup =out-lookup =place =valid? =enabled? =executor)
 
 (define pn                   (create-specification))
 
 ; AST Accessors:
 (define (->Place* n)         (ast-child 'Place* n))
 (define (->Transition* n)    (ast-child 'Transition* n))
 (define (->Token* n)         (ast-child 'Token* n))
 (define (->In n)             (ast-child 'In n))
 (define (->Out n)            (ast-child 'Out n))
 (define (->name n)           (ast-child 'name n))
 (define (->value n)          (ast-child 'value n))
 (define (->place n)          (ast-child 'place n))
 (define (->consumers n)      (ast-child 'consumers n))
 (define (->* n)              (ast-children n))
 (define (<- n)               (ast-parent n))
 
 ; Attribute Accessors:
 (define (=places n)          (att-value 'places n))
 (define (=transitions n)     (att-value 'transitions n))
 (define (=in-arcs n)         (att-value 'in-arcs n))
 (define (=out-arcs n)        (att-value 'out-arcs n))
 (define (=p-lookup n name)   (hash-ref (att-value 'p-lookup n) name #f))
 (define (=t-lookup n name)   (hash-ref (att-value 't-lookup n) name #f))
 (define (=in-lookup n name)  (hash-ref (att-value 'in-lookup n) name #f))
 (define (=out-lookup n name) (hash-ref (att-value 'out-lookup n) name #f))
 (define (=place n)           (att-value 'place n))
 (define (=valid? n)          (att-value 'valid? n))
 (define (=enabled? n)        (att-value 'enabled? n))
 (define (=executor n)        (att-value 'executor n))
 
 ; AST Constructors:
 (define (:AtomicPetrinet p t)
   (create-ast pn 'AtomicPetrinet (list (create-ast-list p) (create-ast-list t))))
 (define (:Place n . t)
   (create-ast pn 'Place (list n (create-ast-list t))))
 (define (:Token v)
   (create-ast pn 'Token (list v)))
 (define (:Transition n i o)
   (create-ast pn 'Transition (list n (create-ast-list i) (create-ast-list o))))
 (define (:Arc p f)
   (create-ast pn 'Arc (list p f)))
 
 ; Support Functions:
 (define (make-symbol-table decls ->key)
   (define table (make-hasheq))
   (for-each (lambda (n) (hash-set! table (->key n) n)) decls)
   table)
 
 (define (specify-analyses)
   (with-specification
    pn
    
    ;;; AST Scheme:
    
    (ast-rule 'AtomicPetrinet->Place*-Transition*)
    (ast-rule 'Place->name-Token*)
    (ast-rule 'Token->value)
    (ast-rule 'Transition->name-Arc*<In-Arc*<Out)
    (ast-rule 'Arc->place-consumers)
    (compile-ast-specifications 'AtomicPetrinet)
    
    ;;; Query Support:
    
    (ag-rule
     places ; List of places of atomic net.
     (AtomicPetrinet   (lambda (n) (->* (->Place* n)))))
    
    (ag-rule
     transitions ; List of transitions of atomic net.
     (AtomicPetrinet   (lambda (n) (->* (->Transition* n)))))
    
    (ag-rule
     in-arcs ; List of ingoing arcs of transition.
     (Transition       (lambda (n) (->* (->In n)))))
    
    (ag-rule
     out-arcs ; List of outgoing arcs of transition.
     (Transition       (lambda (n) (->* (->Out n)))))
    
    ;;; Name Analysis:
    
    (ag-rule
     p-lookup ; Hashmap of all places of atomic net (symbolic name -> place).
     (AtomicPetrinet   (lambda (n) (make-symbol-table (=places n) ->name))))
    
    (ag-rule
     t-lookup ; Hashmap of all transitions of atomic net (symbolic name -> transition).
     (AtomicPetrinet   (lambda (n) (make-symbol-table (=transitions n) ->name))))
    
    (ag-rule
     in-lookup ; Hashmap of all ingoing arcs of transition (symbolic name -> arc).
     (Transition       (lambda (n) (make-symbol-table (=in-arcs n) ->place))))
    
    (ag-rule
     out-lookup ; Hashmap of all outgoing arcs of transition (symbolic name -> arc).
     (Transition       (lambda (n) (make-symbol-table (=out-arcs n) ->place))))
    
    (ag-rule
     place ; Place arc consumes tokens from or produces into (#f if undefined).
     (Arc              (lambda (n) (=p-lookup n (->place n)))))
    
    ;;; Well-formedness Analysis:
    
    (ag-rule
     valid? ; Are a Petri net component and its parts well-formed?
     (Place            (lambda (n) (eq? (=p-lookup n (->name n)) n)))
     (Transition       (lambda (n) (and (eq? (=t-lookup n (->name n)) n)
                                        (andmap =valid? (=in-arcs n))
                                        (andmap =valid? (=out-arcs n)))))
     ((Transition In)  (lambda (n) (and (=place n)
                                        (eq? (=in-lookup n (->place n)) n))))
     ((Transition Out) (lambda (n) (and (=place n)
                                        (eq? (=out-lookup n (->place n)) n))))
     (AtomicPetrinet   (lambda (n) (and (andmap =valid? (=places n))
                                        (andmap =valid? (=transitions n))))))
    
    ;;; Enabled Analysis:
    
    (ag-rule
     enabled? ; Is an arc/transition enabled (if so, return list of tokens it consumes)?
     
     (Arc
      (lambda (n)
        (define consumers (map (lambda (f) (vector #t f)) (->consumers n)))
        (ast-find-child*
         (lambda (i n)
           (define consumer?
             (findf (lambda (c)
                      (and (vector-ref c 0) ((vector-ref c 1) (->value n))))
                    consumers))
           (when consumer?
             (vector-set! consumer? 0 #f)
             (vector-set! consumer? 1 n))
           (and (not (findf (lambda (c) (vector-ref c 0)) consumers))
                (map (lambda (c) (vector-ref c 1)) consumers)))
         (->Token* (=place n)))))
     
     (Transition
      (lambda (n)
        (call/cc
         (lambda (abort)
           (foldl
            (lambda (n result)
              (define enabled? (=enabled? n))
              (if enabled? (append result enabled?) (abort #f)))
            (list)
            (=in-arcs n)))))))
    
    (ag-rule
     executor ; For each transition, function that maps consumed tokens to produced.
     (Transition
      (lambda (n)
        (define producers (map ->consumers (=out-arcs n)))
        (define destinations (map ->Token* (map =place (=out-arcs n))))
        (lambda (consumed-tokens)
          (for-each
           (lambda (producer destination)
             (for-each
              (lambda (value) (rewrite-add destination (:Token value)))
              (apply producer consumed-tokens)))
           producers
           destinations)))))))
