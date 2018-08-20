;;;;;
;;;;; Standard policy applied to all sandboxed processes.
;;;;;
;;;;; Copyright (c) 2014 Apple Inc. All rights reserved.

(version 1)

(define (allowed? op)
  (sbpl-operation-can-return? op 'allow))
(define (denied? op)
  (sbpl-operation-can-return? op 'deny))

;; Allow mach-bootstrap if mach-lookup is ever allowed.
(if (allowed? mach-lookup)
  (allow mach-bootstrap))

;; Allow access to webdavfs_agent if file-read* is always allowed.
;; <rdar://problem/6816031> remove workaround for 6769092
(if (not (denied? file-read*))
  (allow network-outbound
         (regex #"^/private/tmp/\.webdavUDS\.[^/]+$")))

;; Never allow a sandboxed process to open a launchd socket.
(deny network-outbound
      (literal "/private/var/tmp/launchd/sock")
      (regex #"^/private/tmp/launchd-[0-9]+\.[^/]+/sock$"))

;; Always allow a process to signal itself.
(allow signal (target self))
