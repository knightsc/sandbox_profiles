;;;;;
;;;;; Standard policy applied to all sandboxed processes.
;;;;;
;;;;; Copyright (c) 2014 Apple Inc. All rights reserved.

(version 1)

(define (allowed? op)
  (sbpl-operation-can-return? op 'allow))
(define (denied? op)
  (sbpl-operation-can-return? op 'deny))

;; Never allow a sandboxed process to open a launchd socket.
(deny network-outbound
      (literal "/private/var/tmp/launchd/sock")
      (regex #"^/private/tmp/launchd-[0-9]+\.[^/]+/sock$"))

;; Always allow a process to signal itself.
(allow signal (target self))
