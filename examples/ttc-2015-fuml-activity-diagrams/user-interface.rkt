; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger
; Ported to Racket by: Eric Eide

#lang racket

(require "../../racr/core.rkt"
         "../../racr/testing.rkt")
(require (prefix-in pn: "../atomic-petrinets/analyses.rkt")
         (prefix-in pn: "../atomic-petrinets/user-interface.rkt"))
(require "language.rkt"
         "parser.rkt")
(provide run-activity-diagram)
 
 (define (run-activity-diagram diagram-file input-file mode print-trace?) ; Execute diagram.
   (define activity (parse-diagram diagram-file))
   (if print-trace? (activate-tracing) (deactivate-tracing))
   (when input-file
     (for-each
      (lambda (n)
        (define variable (=v-lookup activity (->name n)))
        (unless variable (exception: "Unknown Input"))
        (unless (eq? (->initial variable) Undefined) (exception: "Unknown Input"))
        (rewrite-terminal 'initial variable (->initial n)))
      (parse-diagram-input input-file)))
   (unless (andmap (lambda (n) (not (eq? (->initial n) Undefined))) (=variables activity))
     (exception: "Missing Input"))
   (when (> mode 1)
     (unless (=valid? activity) (exception: "Invalid Diagram"))
     (when (> mode 2)
       (let ((net (=petrinet activity)))
         (when (> mode 3)
           (unless (pn:=valid? net) (exception: "Invalid Diagram"))
           (when (> mode 4)
             (for-each pn:=enabled? (pn:=transitions net))
             (when (> mode 5)
               (trace (->name (=initial activity)))
               (if (= mode 6)
                   (pn:run-petrinet! net)
                   (do ((enabled (filter pn:=enabled? (pn:=transitions net))
                                 (filter pn:=enabled? (pn:=transitions net))))
                     ((null? enabled))
                     (for-each pn:fire-transition! enabled)))
               (for-each
                (lambda (n) (trace (->name n) " = " ((=v-accessor n))))
                (=variables activity)))))))))
 
 (pn:initialise-petrinet-language)
