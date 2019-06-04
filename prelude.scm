;;;;;; Sandbox Profile Language prelude

;;; SBPL library functions

;; param -- get the value of a compile-time parameter, or false if undefined
(define (param key)
  (let ((value (assoc key *params*)))
    (and value (cdr value))))
