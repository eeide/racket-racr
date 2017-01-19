; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger
; Ported to Racket by: Eric Eide

#lang racket

(require rackunit)
(require "../../../racr/core.rkt")
(require "../user-interface.rkt"
         "../analyses.rkt")

(define (make-net-1)
  (compose-petrinets: ; Not glued: (in: c | out: c | in/out: a)
   (petrinet: a (in* in/out) (in/out) ((in*) (in/out)))
   (compose-petrinets:
    (petrinet: b (in/out*) (in/out*) ((in/out*)))
    (petrinet: c (in) (out* out) ((out*) (in) (out)))
    ((c out*) (b in/out*)))
   ((b in/out*) (a in*))))

(define (make-net-2)
  (compose-petrinets: ; Not glued: (in: f | out: d | in/out: e)
   (compose-petrinets:
    (petrinet: d (in*) (out) ((in*) (out)))
    (petrinet: e (in/out* in/out) (in/out* in/out) ((in/out*) (in/out)))
    ((e in/out*) (d in*)))
   (petrinet: f (in) (out*) ((in) (out*)))
   ((f out*) (e in/out*))))

(define (run-error-cases)
  ;;; Atomic Petri nets:
  
  (check-exn exn:fail:user? ; Non-unique in-ports
   (lambda ()
   (petrinet: net (A A) () ((A)))))
  (check-exn exn:fail:user? ; Non-unique out-ports
   (lambda ()
   (petrinet: net () (A A) ((A)))))
  (check-exn exn:fail:user? ; Unknown in-port
   (lambda ()
   (petrinet: net (A) () ())))
  (check-exn exn:fail:user? ; Unknown out-port
   (lambda ()
   (petrinet: net () (A) ())))
  
  ;;; Flat composition structure:
  
  (check-exn exn:fail:user? ; Non-unique subnets
   (lambda ()
   (compose-petrinets:
    (petrinet: a () () ())
    (petrinet: a () () ()))))
  (check-exn exn:fail:user? ; Unknown in-port glueing
   (lambda ()
   (compose-petrinets:
    (petrinet: a (A) () ((A)))
    (petrinet: b () (A) ((A)))
    ((b A) (a A*)))))
  (check-exn exn:fail:user? ; Unknown out-port glueing
   (lambda ()
   (compose-petrinets:
    (petrinet: a (A) () ((A)))
    (petrinet: b () (A) ((A)))
    ((b A*) (a A)))))
  (check-exn exn:fail:user? ; in- to in-port glueing
   (lambda ()
   (compose-petrinets:
    (petrinet: a (A) () ((A)))
    (petrinet: b (A) () ((A)))
    ((b A) (a A)))))
  (check-exn exn:fail:user? ; out- to out-port glueing
   (lambda ()
   (compose-petrinets:
    (petrinet: a () (A) ((A)))
    (petrinet: b () (A) ((A)))
    ((b A) (a A)))))
  (check-exn exn:fail:user? ; Twisted glueing (in-port as out-port and vice versa)
   (lambda ()
   (compose-petrinets:
    (petrinet: a (A) () ((A)))
    (petrinet: b () (A) ((A)))
    ((a A) (b A)))))
  (check-exn exn:fail:user? ; Fusion of places of the same atomic net
   (lambda ()
   (compose-petrinets:
    (petrinet: a (A) (A) ((A)))
    (petrinet: b (A) (B) ((A) (B)))
    ((a A) (b A))
    ((b B) (a A)))))
  
  ;;; Nested composition structure:
  
  (let ((net-1 ; Non-unique subnets
         (compose-petrinets:
          (petrinet: a () () ())
          (compose-petrinets:
           (petrinet: b () () ())
           (petrinet: c () () ()))))
        (net-2
         (compose-petrinets:
          (petrinet: d () () ())
          (petrinet: b () () ()))))
    (check-exn exn:fail:user?
     (lambda ()
     (compose-petrinets: net-1 net-2))))
  
  (check-exn exn:fail:user? ; Unknown in-port glueing
   (lambda ()
   (compose-petrinets: (make-net-1) (make-net-2) ((c out) (f /in/)))))
  (check-exn exn:fail:user? ; Unknown out-port glueing
   (lambda ()
   (compose-petrinets: (make-net-1) (make-net-2) ((d /out/) (c in)))))
  (check-exn exn:fail:user? ; in- to in-port glueing
   (lambda ()
   (compose-petrinets: (make-net-1) (make-net-2) ((c in) (f in)))))
  (check-exn exn:fail:user? ; out- to out-port glueing
   (lambda ()
   (compose-petrinets: (make-net-1) (make-net-2) ((d out) (c out)))))
  (check-exn exn:fail:user? ; Twisted glueing (in-port as out-port and vice versa)
   (lambda ()
   (compose-petrinets: (make-net-1) (make-net-2) ((f in) (c out)))))
  (check-exn exn:fail:user? ; Fusion of places of the same atomic net
   (lambda ()
   (compose-petrinets: (make-net-1) (make-net-2)
                       ((c out) (e in/out))
                       ((e in/out) (a in/out))
                       ((a in/out) (c in)))))
  (check-exn exn:fail:user? ; Glueing of shadowed in-port
   (lambda ()
   (compose-petrinets: (make-net-1) (make-net-2)
                       ((a in/out) (e in/out*)))))
  (check-exn exn:fail:user? ; Glueing of shadowed out-port
   (lambda ()
   (compose-petrinets: (make-net-1) (make-net-2)
                       ((c out*) (f in))))))

(define (run-correct-cases)
  (make-net-1)
  (make-net-2)
  (compose-petrinets: (make-net-1) (make-net-2)
                      ((d out) (a in/out))
                      ((a in/out) (e in/out))
                      ((e in/out) (c in))
                      ((c out) (f in))))

(define (run-tests)
  (run-error-cases)
  (run-correct-cases))

(initialise-petrinet-language)
(run-tests)
