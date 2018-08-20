;;;;;
;;;;; Standard policy applied to all sandboxed processes.
;;;;;
;;;;; Copyright (c) 2014 Apple Inc. All rights reserved.

(version 1)

(define (allowed? op)
  (sbpl-operation-can-return? op 'allow))
(define (denied? op)
  (sbpl-operation-can-return? op 'deny))

;; Always allow a process to signal itself.
(allow signal (target self))

;; Always allow a process to get its own task-name port.
(allow mach-task-name (target self))
