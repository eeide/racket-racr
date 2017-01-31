; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger
; Ported to Racket by: Eric Eide

#lang racket

(require rackunit)
(require "../../racr/core.rkt"
         "../../racr/testing.rkt")
(require "analyses.rkt")
(provide
         initialise-petrinet-language petrinet: transition: exception:
         fire-transition! run-petrinet! interpret-petrinet!
         #;petrinets-exception? check-marking check-enabled)
 
 ;;; Exceptions:
 
 #;(define-condition-type petrinets-exception
   &violation
   make-petrinets-exception
   petrinets-exception?)
 
 (define (exception: message)
   (raise-user-error 'petrinets "~s" message))
 
 ;;; Syntax:
 
 (define-syntax petrinet:
   (syntax-rules ()
     ((_ ((place start-marking ...) ...)
         transition ... )
      (let ((net
             (:AtomicPetrinet
              (list (:Place 'place (:Token start-marking) ...)
                    ...)
              (list transition
                    ...))))
        (unless (=valid? net)
          (exception: "Cannot construct Petri net; The net is not well-formed."))
        net))))
 
 (define-syntax transition:
   (syntax-rules ()
     ((_ name
         ((input-place (variable matching-condition) ...) ...)
         ((output-place to-produce ...) ...))
      (:Transition
       'name
       (list (:Arc
              'input-place
              (list (lambda (variable) matching-condition) ...))
             ...)
       (list (:Arc
              'output-place
              (lambda (variable ... ...) (list to-produce ...)))
             ...)))))
 
 ;;; Execution:
 
 (define (run-petrinet! petrinet)
   (unless (=valid? petrinet)
     (exception: "Cannot run Petri Net; The given net is not well-formed."))
   (let ((enabled? (findf =enabled? (=transitions petrinet))))
     (when enabled?
       (fire-transition! enabled?)
       (run-petrinet! petrinet))))
 
 (define (fire-transition! transition)
   (define enabled? (=enabled? transition))
   (unless enabled?
     (exception: "Cannot fire transition; The transition is not enabled."))
   (let ((consumed-tokens (map ->value enabled?)))
     (for-each rewrite-delete enabled?)
     ((=executor transition) consumed-tokens)))
 
 ;;; REPL Interpreter:
 
 (define (interpret-petrinet! net)
   (unless (=valid? net)
     (exception: "Cannot interpret Petri Net; The given net is not well-formed."))
   (display "Marking:\n")
   (map
    (lambda (n)
      (display "  ") (display (->name n)) (display ": ")
      (display (map ->value (->* (->Token* n)))) (display "\n"))
    (=places net))
   (display "Enabled:\n  ")
   (display (map ->name (filter =enabled? (=transitions net))))
   (display "\nFire (#f to terminate, EOF to abort):  ")
   (let ((input (read)))
     (cond
       ((eof-object? input) #t)
       ((not input) #f)
       (else
        (let ((to-fire? (=t-lookup net input)))
          (unless to-fire?
            (exception: "Cannot interpret Petri Net; Undefined transition to execute."))
          (display "  ") (display (->name to-fire?)) (display "\n")
          (fire-transition! to-fire?)
          (interpret-petrinet! net))))))
 
 ;;; Testing:
 
 (define-simple-check (check-marking net marking) ; Each marking is a list of a place followed by its tokens.
   (define marked (map (lambda (m) (=p-lookup net (car m))) marking))
   (define marked-marking (map cdr marking))
   (define !marked (filter (lambda (n) (not (memq n marked))) (=places net)))
   (define !marked-marking (map (lambda (n) (list)) !marked))
   (define (check-place place expected-tokens)
     (define given-values (list->vector (map ->value (->* (->Token* place)))))
     (struct nil-record ())
     (define Ok (nil-record))
     (for-each
      (lambda (expected-token)
        (let ((value-found? (vector-member expected-token given-values)))
          (unless value-found?
            (fail-check))
          (vector-set! given-values value-found? Ok)))
      expected-tokens)
     (unless (eq? (vector-length given-values)
                  (vector-count nil-record? given-values))
       (fail-check)))
   (for-each check-place marked marked-marking)
   (for-each check-place !marked !marked-marking)
   #t)
 
 (define-simple-check (check-enabled net enabled)
   (define t-enabled (map (lambda (t) (=t-lookup net t)) enabled))
   (define t-!enabled (filter (lambda (t) (not (memq t t-enabled))) (=transitions net)))
   (unless (andmap =enabled? t-enabled)
     (fail-check))
   (unless (andmap (lambda (t) (not (=enabled? t))) t-!enabled)
     (fail-check))
   #t)
 
 ;;; Initialisation:
 
 (define (initialise-petrinet-language)
   (when (= (specification->phase pn) 1)
     (specify-analyses)
     (compile-ag-specifications pn)))