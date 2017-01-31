; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger
; Ported to Racket by: Eric Eide

#lang racket

(require rackunit)
(require "../../../racr/testing.rkt")
(require "../user-interface.rkt")

(define (run-error-cases)
  (check-exn exn:fail:user? ; Non-unique places.
   (lambda ()
   (petrinet: ((A) (A)))))
  (check-exn exn:fail:user? ; Non-unique transitions.
   (lambda ()
   (petrinet: ((A))
              (transition: a () ())
              (transition: a () ()))))
  (check-exn exn:fail:user? ; Unknown source place.
   (lambda ()
   (petrinet: ()
              (transition: a ((A)    ) (       )))))
  (check-exn exn:fail:user? ; Unknown target place.
   (lambda ()
   (petrinet: ()
              (transition: a (       ) ((A)    )))))
  (check-exn exn:fail:user? ; Non-unique ingoing arcs.
   (lambda ()
   (petrinet: ((A))
              (transition: a ((A) (A)) (       )))))
  (check-exn exn:fail:user? ; Non-unique outgoing arcs.
   (lambda ()
   (petrinet: ((A))
              (transition: a (       ) ((A) (A)))))))

(define (run-correct-cases)
  (petrinet: (         )                                                          )  ; Empty net.
  (petrinet: ((A) (B 1))                                                          )  ; No transitions.
  (petrinet: ((A) (B 1)) (transition: a (                         ) (           )))  ; No arcs.
  (petrinet: ((A) (B 1)) (transition: a ((A) (B (a 1))            ) (           )))  ; No outgoing arcs.
  (petrinet: ((A) (B 1)) (transition: a ((A) (B (a 1) (b 2) (c 3))) ((A) (B 1 b))))  ; Weights & colours.
  (petrinet: ((A) (B 1)) (transition: a (                         ) ((A) (B 1  ))))) ; No ingoing arcs.
  
(define (run-tests)
  (run-error-cases)
  (run-correct-cases))

(initialise-petrinet-language)
(run-tests)
