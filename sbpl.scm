;;;;;; Sandbox Profile Language stub

;;; This stub is loaded before the sandbox profile is evaluated.  When version
;;; is called, the SBPL prelude and the appropriate SBPL version library are
;;; loaded, which together implement the profile language.  These modules build
;;; a *rules* table that maps operation codes to lists of rules of the form
;;;   RULE -> TEST | JUMP
;;;   TEST -> (filter action . modifiers)
;;;   JUMP -> (#f . operation)
;;; The result of an operation is decided by the first test with a filter that
;;; matches.  Filter can be #t, in which case the test always matches.  A jump
;;; causes evaluation to continue with the rules for another operation.  The
;;; last rule in the list must either be a test that always matches or a jump.

(define (version n)
  (case n
    (( 1) (%version-1))
    (else (error "unsupported version" n)))
  (set! version
        (lambda (o)
          (if (not (= n o))
              (error "only one version may be specified")))))

(define (%finalize)
  (error "no version specified"))
