; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger
; Ported to Racket by: Eric Eide

#lang racket

(require racket/cmdline)
(require "user-interface.rkt")

(command-line
 #:args (diagram input? mode print-trace?)
 (run-activity-diagram diagram
                       (if (string=? input? ":false:")
                           #f
                           input?)
                       (string->number mode)
                       (not (string=? print-trace? ":false:"))))
