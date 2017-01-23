; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger
; Ported to Racket by: Eric Eide

#lang racket

(require racket/cmdline)
(require rackunit)
(require "../../racr/testing.rkt")
(require "main.rkt"
         "exception-api.rkt")

(command-line
 #:args (program incorrect?)
 (if (string=? incorrect? ":true:")
     (check-exn exn:fail:user? (lambda () (siple-interpret program)))
     (siple-interpret program)))
