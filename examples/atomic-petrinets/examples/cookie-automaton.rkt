; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger
; Ported to Racket by: Eric Eide

; Specification of the cookie automaton given on page 21, Figure 2.1 in
; 
;               "Petrinetze: Modellierungstechnik, Analysemethoden, Fallstudien"
;                                     Wolfgang Reisig
;                                  Vieweg+Teubner, 2010
;                                    978-3-8348-1290-2

#lang racket

(require rackunit)
(require "../../../racr/core.rkt"
         "../../../racr/testing.rkt")
(require "../user-interface.rkt"
         "../analyses.rkt")

(define Box 'Box)
(define Box* 'Box*)
(define Euro 'Euro)
(define Token 'Token)

(define (make-cookie-automaton)
  (petrinet:
   ; Places with start marking:
   ((H Box Box Box Box Box Box* Box*)           ; H with tokens of two different kind
    (D Token)                                   ; D with 'Token token
    (G Token)                                   ; G with 'Token token
    (E 7)                                       ; E with integer token of value 7
    (A) (B) (C) (F))                            ; All other places have no tokens.
   
   ; Transitions:
   (transition: c
                ((D (token (eq? token Token)))) ; Consume 'Token token from D.
                ((A 'Euro)))                    ; Produce 'Euro token in A.
   
   (transition: e
                ((A (euro (eq? euro Euro))))    ; Consume 'Euro token from A.
                ((D 'Token)))                   ; Produce 'Token token in D.
   
   (transition: a
                ((A (euro (eq? euro Euro)))     ; Consume 'Euro token from A.
                 (E (x (>= x 2)))               ; Consume an x >= 2 token from E.
                 (G (token (eq? token Token)))) ; Consume 'Token token from G.
                ((D 'Token)                     ; Produce 'Token token in D.
                 (E (- x 2))                    ; Put x decremented by two in E.
                 (F 'Euro)                      ; Produce 'Euro token in F.
                 (B 'Token)))                   ; Produce 'Token token in B.
   
   (transition: b
                ((B (token (eq? token Token)))  ; Consume 'Euro token from B.
                 (H (y #t) (z #t)))             ; Consume two arbitrary tokens from H.
                ((G 'Token)                     ; Produce 'Token token in G.
                 (C y z)))                      ; Put tokens consumed from H in C.
   
   (transition: d
                ((C (y #t)))                    ; Consume arbitrary token from C.
                ())))                           ; d is cold-transition: produce nothing.									; d is cold-transition. It produces nothing.)

(define (run-tests)
  ; Test fixture: Transition b nondeterministicly selects boxes from H!
  ;  To test markings without an explosion of possible combinations...
  (set! Box* Box) ; ...rectify the box types.
  
  (let ((net (make-cookie-automaton)))
    (check-marking
     net
     (list
      (list 'D Token)
      (list 'E 7)
      (list 'G Token)
      (list 'H Box Box Box Box Box Box* Box*)))
    (check-enabled net '(c))
    (fire-transition! (=t-lookup net 'c))
    (check-marking
     net
     (list
      (list 'A Euro)
      (list 'E 7)
      (list 'G Token)
      (list 'H Box Box Box Box Box Box* Box*)))
    (check-enabled net '(a e))
    (fire-transition! (=t-lookup net 'a))
    (check-marking
     net
     (list
      (list 'B Token)
      (list 'D Token)
      (list 'E 5)
      (list 'F Euro)
      (list 'H Box Box Box Box Box Box* Box*)))
    (check-enabled net '(b c))
    (fire-transition! (=t-lookup net 'c))
    (check-marking
     net
     (list
      (list 'A Euro)
      (list 'B Token)
      (list 'E 5)
      (list 'F Euro)
      (list 'H Box Box Box Box Box Box* Box*)))
    (check-enabled net '(b e))
    (fire-transition! (=t-lookup net 'e))
    (check-marking
     net
     (list
      (list 'B Token)
      (list 'D Token)
      (list 'E 5)
      (list 'F Euro)
      (list 'H Box Box Box Box Box Box* Box*)))
    (check-enabled net '(b c))
    (fire-transition! (=t-lookup net 'c))
    (check-marking
     net
     (list
      (list 'A Euro)
      (list 'B Token)
      (list 'E 5)
      (list 'F Euro)
      (list 'H Box Box Box Box Box Box* Box*)))
    (check-enabled net '(b e))
    (fire-transition! (=t-lookup net 'b))
    (check-marking
     net
     (list
      (list 'A Euro)
      (list 'C Box Box)
      (list 'E 5)
      (list 'F Euro)
      (list 'G Token)
      (list 'H Box Box Box Box* Box*)))
    (check-enabled net '(a d e))
    (fire-transition! (=t-lookup net 'e))
    (check-marking
     net
     (list
      (list 'C Box Box)
      (list 'D Token)
      (list 'E 5)
      (list 'F Euro)
      (list 'G Token)
      (list 'H Box Box Box Box* Box*)))
    (check-enabled net '(c d))
    (fire-transition! (=t-lookup net 'c))
    (check-marking
     net
     (list
      (list 'A Euro)
      (list 'C Box Box)
      (list 'E 5)
      (list 'F Euro)
      (list 'G Token)
      (list 'H Box Box Box Box* Box*)))
    (check-enabled net '(a d e))
    (fire-transition! (=t-lookup net 'd))
    (check-marking
     net
     (list
      (list 'A Euro)
      (list 'C Box)
      (list 'E 5)
      (list 'F Euro)
      (list 'G Token)
      (list 'H Box Box Box Box* Box*)))
    (check-enabled net '(a e d))
    (fire-transition! (=t-lookup net 'd))
    (check-marking
     net
     (list
      (list 'A Euro)
      (list 'E 5)
      (list 'F Euro)
      (list 'G Token)
      (list 'H Box Box Box Box* Box*)))
    (check-enabled net '(a e))
    (fire-transition! (=t-lookup net 'a))
    (check-marking
     net
     (list
      (list 'B Token)
      (list 'D Token)
      (list 'E 3)
      (list 'F Euro Euro)
      (list 'H Box Box Box Box* Box*)))
    (check-enabled net '(b c))
    (fire-transition! (=t-lookup net 'b))
    (check-marking
     net
     (list
      (list 'C Box Box)
      (list 'D Token)
      (list 'E 3)
      (list 'F Euro Euro)
      (list 'G Token)
      (list 'H Box Box* Box*)))
    (check-enabled net '(c d))
    (fire-transition! (=t-lookup net 'c))
    (check-marking
     net
     (list
      (list 'A Euro)
      (list 'C Box Box)
      (list 'E 3)
      (list 'F Euro Euro)
      (list 'G Token)
      (list 'H Box Box* Box*)))
    (check-enabled net '(a d e))
    (fire-transition! (=t-lookup net 'a))
    (check-marking
     net
     (list
      (list 'B Token)
      (list 'C Box Box)
      (list 'D Token)
      (list 'E 1)
      (list 'F Euro Euro Euro)
      (list 'H Box Box* Box*)))
    (check-enabled net '(b c d))
    (fire-transition! (=t-lookup net 'b))
    (check-marking
     net
     (list
      (list 'C Box* Box Box Box)
      (list 'D Token)
      (list 'E 1)
      (list 'F Euro Euro Euro)
      (list 'G Token)
      (list 'H Box*)))
    (check-enabled net '(c d))
    (fire-transition! (=t-lookup net 'c))
    (check-marking
     net
     (list
      (list 'A Euro)
      (list 'C Box* Box Box Box)
      (list 'E 1)
      (list 'F Euro Euro Euro)
      (list 'G Token)
      (list 'H Box*)))
    (check-enabled net '(d e))
    (fire-transition! (=t-lookup net 'd))
    (check-marking
     net
     (list
      (list 'A Euro)
      (list 'C Box* Box Box)
      (list 'E 1)
      (list 'F Euro Euro Euro)
      (list 'G Token)
      (list 'H Box*)))
    (check-enabled net '(d e))
    (fire-transition! (=t-lookup net 'd))
    (check-marking
     net
     (list
      (list 'A Euro)
      (list 'C Box* Box)
      (list 'E 1)
      (list 'F Euro Euro Euro)
      (list 'G Token)
      (list 'H Box*)))
    (check-enabled net '(d e))
    (fire-transition! (=t-lookup net 'd))
    (check-marking
     net
     (list
      (list 'A Euro)
      (list 'C Box*)
      (list 'E 1)
      (list 'F Euro Euro Euro)
      (list 'G Token)
      (list 'H Box*)))
    (check-enabled net '(d e))
    (fire-transition! (=t-lookup net 'd))
    (check-marking
     net
     (list
      (list 'A Euro)
      (list 'E 1)
      (list 'F Euro Euro Euro)
      (list 'G Token)
      (list 'H Box*)))
    (check-enabled net '(e))
    (fire-transition! (=t-lookup net 'e))
    (check-marking
     net
     (list
      (list 'D Token)
      (list 'E 1)
      (list 'F Euro Euro Euro)
      (list 'G Token)
      (list 'H Box*)))
    (check-enabled net '(c))
    (fire-transition! (=t-lookup net 'c))
    (check-enabled net '(e))
    (check-marking
     net
     (list
      (list 'A Euro)
      (list 'E 1)
      (list 'F Euro Euro Euro)
      (list 'G Token)
      (list 'H Box*)))
    (fire-transition! (=t-lookup net 'e))
    (check-marking
     net
     (list
      (list 'D Token)
      (list 'E 1)
      (list 'F Euro Euro Euro)
      (list 'G Token)
      (list 'H Box*)))
    (check-enabled net '(c))
    (fire-transition! (=t-lookup net 'c))
    (check-marking
     net
     (list
      (list 'A Euro)
      (list 'E 1)
      (list 'F Euro Euro Euro)
      (list 'G Token)
      (list 'H Box*)))
    (check-enabled net '(e))
    (fire-transition! (=t-lookup net 'e))
    ; ...
    (check-enabled net '(c))
    (check-marking
     net
     (list
      (list 'D Token)
      (list 'E 1)
      (list 'F Euro Euro Euro)
      (list 'G Token)
      (list 'H Box*))))
  
  (let ((net (make-cookie-automaton)))
    (rewrite-delete (=t-lookup net 'e))
    (run-petrinet! net)
    (check-marking
     net
     (list
      (list 'A Euro)
      (list 'E 1)
      (list 'F Euro Euro Euro)
      (list 'G Token)
      (list 'H Box*))))
  
  (let ((net (make-cookie-automaton)))
    (rewrite-delete (=p-lookup net 'A))
    (check-exn exn:fail:user? (lambda () (run-petrinet! net))))
  
  (set! Box* 'Box*)) ; Undo test fixture.

(initialise-petrinet-language)
(run-tests)
