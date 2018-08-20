;;;;;; Sandbox Profile Language version 1

;;; Directives

;; Define a pair of lists of file dependencies; the car is a list of files that
;; were successfully opened for reading, and the cdr is a list of files that
;; could not be opened for reading.  These dependencies are used to determine
;; when a cached profile is invalid.  The pair is set to #f if any files were
;; opened for writing (because profiles that write to disk cannot be cached)
;; or if the full path to a file opened for reading cannot be determined.
(define *dependencies* (cons '() '()))
;; Wrap standard I/O procedures to update the dependency lists.
(let ((old-load load)
      (old-open-input-file open-input-file)
      (old-open-output-file open-output-file)
      ;; Add an element to a list if the element is not already in the list.
      (push-unique (lambda (elt lst)
                     (let loop ((remaining lst))
                       (cond
                        ((null? remaining) (cons elt lst))
                        ((equal? elt (car remaining)) lst)
                        (else (loop (cdr remaining))))))))
  (set! load
        (lambda (path)
          (set! *dependencies*
                (and *dependencies*
                     (< 0 (string-length path))
                     (eqv? #\/ (string-ref path 0))
                     (cons (push-unique path (car *dependencies*))
                           (cdr *dependencies*))))
          (old-load path)))
  (set! open-input-file
        (lambda (path)
          (let ((port (old-open-input-file path)))
            (set! *dependencies*
                  (and *dependencies*
                       (< 0 (string-length path))
                       (eqv? #\/ (string-ref path 0))
                       (if port
                           (cons (push-unique path (car *dependencies*))
                                 (cdr *dependencies*))
                           (cons (car *dependencies*)
                                 (push-unique path (cdr *dependencies*))))))
            port)))
  (set! open-output-file
        (lambda (path)
          (set! *dependencies* #f)
          (old-open-output-file path))))
;; Define the import directive.
(define (import path)
  (define import-dirs
    (if (param "IMPORT_DIR")
        (list (param "IMPORT_DIR"))
        (list "/System/Library/Sandbox/Profiles"
              "/usr/share/sandbox"
              "/Library/Sandbox/Profiles")))
  (if (or (= 0 (string-length path))
          (eqv? #\/ (string-ref path 0)))
      ;; Absolute path, load it directly.
      (load path)
      ;; Relative path, search import-dirs.
      (let search ((dirs import-dirs))
        (if (null? dirs)
            (error (string-append "unable to open " path)))
        ;; Attempt to open the path relative to the next dir in the list.
        (let* ((try (string-append (car dirs)
                                   "/"
                                   path))
               (tried (open-input-file try)))
          ;; Load the file if it could be opened, otherwise keep searching.
          (if tried
              (begin (close-input-port tried)
                     (load try))
              (search (cdr dirs)))))))

;; Define the trace directive.
(define *trace* #f)
(define (trace path)
  (cond
   ((not (string? path))
    (error "argument to trace must be a string"))
   (*trace*
    (error "only one trace path may be specified"))
   (else
    (set! *trace* path))))

;; Define the record directive.
(define *record* #f)
(define (record path)
  (set! *record* path))

;; Define directives for optimizing the compiled sandbox profile.
(define *eliminate-duplicate-rules* #f)
(define (eliminate-duplicate-rules)
  (set! *eliminate-duplicate-rules* #t))

;; Define directives for configuring sandbox options.
(define *callouts* #t)
(define *full-symbolication* #t)
(define (disable-callouts)
  (set! *callouts* #f))
(define (disable-full-symbolication)
  (set! *full-symbolication* #f))

;;; Actions

;; The %remove function returns the elements of a list not satisfying a
;; predicate.
(define (%remove pred lst)
  (cond
   ((null? lst)
    lst)
   ((pred (car lst))
    (%remove pred (cdr lst)))
   (else
    (cons (car lst)
          (%remove pred (cdr lst))))))

;; The %action function takes the name of an action and returns a function
;; that performs the job of that action.
(define (%action a)
  ;; Collect an unordered list of rule arguments into a list of operations,
  ;; filters, and modifiers.
  (define (collect l o f m)
    (cond
     ((null? l)
      (list o f m))
     ((list? (car l))
      (case (caar l)
        ((operation) (collect (cdr l) (cons (car l) o) f m))
        ((filter   ) (collect (cdr l) o (cons (car l) f) m))
        ((modifier ) (collect (cdr l) o f (cons (car l) m)))
        (else        (error "illegal argument:" (car l)))))
     (else
      (error "illegal argument:" (car l)))))
  (lambda args
    (let* ((collected (collect args '() '() '()))
           (os (car collected))
           ;; If there are multiple filter arguments, combine them into a
           ;; single any filter.  If there are no filter arguments, use #t.
           (fs (if (pair? (cadr collected))
                   (apply require-any (cadr collected))
                   #t))
           (ms (%remove (lambda (m)
                          (eq? 'deprecated (%m/name m)))
                        (caddr collected))))
      ;; Verify that at least one operation was provided.
      (if (null? os)
          (error "at least one operation required"))
      ;; Verify that no modifier appears more than once and every modifier
      ;; applies to this action.
      (let check-modifiers ((check ms)
                            (seen '()))
        (if (pair? check)
            (let ((m (car check)))
              (cond
               ((memq (%m/name m)
                      seen)
                (error (string-append
                        (symbol->string (%m/name m))
                        " modifier can only appear once per rule")))
               ((not ((%m/check m) a))
                (error (string-append
                        (symbol->string (%m/name m))
                        " modifier does not apply to "
                        (symbol->string a)
                        " action")))
               (else
                (check-modifiers (cdr check)
                                 (cons (%m/name m)
                                       seen)))))))
      ;; Iterate through the provided operations.
      (do ((remaining os (cdr remaining)))
          ((null? remaining))
        (define o (car remaining))
        (define rules (vector-ref *rules* (%o/code o)))
        ;; Verify that the filter applies to this operation.
        ;; This operates recursively on combination filters.
        (define (check-filter f)
          (if (not (eq? #t f))
              (if (eq? 'combination (%f/type f))
                  (map check-filter (%f/args f))
                  (if (not (memq (%f/type f)
                                 (%o/filters o)))
                      (error (string-append
                              (symbol->string (%f/type f))
                              " filter does not apply to "
                              (symbol->string (%o/name o))
                              " operation"))))))
        (check-filter fs)
        ;; Verify that each modifier applies to this operation.
        (define (check-modifier m)
          (if (not (memq (%m/name m)
                         (%o/modifiers o)))
              (error (string-append
                      (symbol->string (%m/name m))
                      " modifier does not apply to "
                      (symbol->string (%o/name o))
                      " operation"))))
        (map check-modifier ms)
        ;; Add the rule into the rule table.
        (cond
         ((eq? #t fs)
          ;; Replace the unconditional rule.
          (vector-set! *rules*
                       (%o/code o)
                       (reverse (cons (cons #t (cons a ms))
                                      (cdr (reverse rules))))))
         ((and (pair? (caar rules))
               (equal? (cdar rules)
                       (cons a ms)))
          ;; Combine the filter with the last rule.
          (set-car! (car rules)
                    (require-any fs (caar rules))))
         (else
          ;; Insert a new rule.
          (vector-set! *rules*
                       (%o/code o)
                       (cons (cons fs (cons a ms))
                             rules))))))))

;; Define the SBPL actions.
(define allow (%action 'allow))
(define deny (%action 'deny))

;;; Operations

;; Operations have the form (operation name code filters . modifiers)
;; e.g. (operation file* (path) (send-signal no-report) 1 0)
(define %o/name cadr)                   ; operation name
(define %o/code caddr)                  ; operation code
(define %o/filters cadddr)              ; compatible filters
(define %o/modifiers cddddr)            ; compatible modifiers

;; The %operations macro takes a list of operations and defines them.
(macro (%operations form)
  (define (operation name filters modifiers action code . ancestors)
    `(begin
       (define ,name '(operation ,name ,code ,filters . ,modifiers))
       (vector-set! *rules*
                    ,code
                    (list ',(if action
                                (list #t action)
                                (cons #f (car ancestors)))))
       (vector-set! *operations* ,code ,name)))
  `(begin
     ;; Define the rule table.
     (define *rules* (make-vector ,(length (cdr form))))
     ;; Define a table of all the operations.
     (define *operations* (make-vector ,(length (cdr form))))
     .
     ;; Define each operation, priming the rule table with jumps to more
     ;; general operations when no default action is given.
     ,(map (lambda (o)
             (apply operation o))
           (cdr form))))

;; Invoke the %operations macro.
(%operations
  (default
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    deny
    0)
  (appleevent-send
    (debug-mode entitlement extension process ae-destination)
    (send-signal report no-report deprecated rootless)
    #f
    1 0)
  (authorization-right-obtain
    (debug-mode entitlement extension process auth-right-name)
    (send-signal report no-report deprecated rootless)
    #f
    2 0)
  (device*
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    3 0)
  (device-camera
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    4 3 0)
  (device-microphone
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    5 3 0)
  (distributed-notification-post
    (debug-mode entitlement extension process notification)
    (send-signal report no-report deprecated rootless)
    #f
    6 0)
  (file*
    (debug-mode entitlement extension process path file-mode vnode-type device)
    (send-signal report no-report deprecated rootless)
    #f
    7 0)
  (file-chroot
    (debug-mode entitlement extension process path file-mode vnode-type device)
    (send-signal report no-report deprecated rootless)
    #f
    8 7 0)
  (file-ioctl
    (debug-mode entitlement extension process path file-mode vnode-type device ioctl)
    (send-signal report no-report deprecated rootless)
    #f
    9 7 0)
  (file-issue-extension
    (debug-mode entitlement extension process path file-mode vnode-type device extension-class)
    (send-signal report no-report deprecated rootless)
    #f
    10 7 0)
  (file-map-executable
    (debug-mode entitlement extension process path file-mode vnode-type device)
    (send-signal report no-report deprecated rootless)
    allow
    11 7 0)
  (file-mknod
    (debug-mode entitlement extension process path file-mode vnode-type device)
    (send-signal report no-report deprecated rootless)
    #f
    12 7 0)
  (file-mount
    (debug-mode entitlement extension process path file-mode vnode-type device)
    (send-signal report no-report deprecated rootless)
    #f
    13 7 0)
  (file-read*
    (debug-mode entitlement extension process path file-mode vnode-type device cache-safe)
    (send-signal report no-report deprecated rootless)
    #f
    14 7 0)
  (file-read-data
    (debug-mode entitlement extension process path file-mode vnode-type device cache-safe)
    (send-signal report no-report deprecated rootless)
    #f
    15 14 7 0)
  (file-read-metadata
    (debug-mode entitlement extension process path file-mode vnode-type device cache-safe)
    (send-signal report no-report deprecated rootless)
    #f
    16 14 7 0)
  (file-read-xattr
    (debug-mode entitlement extension process path file-mode vnode-type device cache-safe xattr)
    (send-signal report no-report deprecated rootless)
    #f
    17 14 7 0)
  (file-revoke
    (debug-mode entitlement extension process path file-mode vnode-type device)
    (send-signal report no-report deprecated rootless)
    #f
    18 7 0)
  (file-search
    (debug-mode entitlement extension process path file-mode vnode-type device)
    (send-signal report no-report deprecated rootless)
    #f
    19 7 0)
  (file-unmount
    (debug-mode entitlement extension process path file-mode vnode-type device)
    (send-signal report no-report deprecated rootless)
    #f
    20 7 0)
  (file-write*
    (debug-mode entitlement extension process path file-mode vnode-type device)
    (send-signal report no-report deprecated rootless)
    #f
    21 7 0)
  (file-write-create
    (debug-mode entitlement extension process path file-mode vnode-type device)
    (send-signal report no-report deprecated rootless)
    #f
    22 21 7 0)
  (file-write-data
    (debug-mode entitlement extension process path file-mode vnode-type device)
    (send-signal report no-report deprecated rootless)
    #f
    23 21 7 0)
  (file-write-flags
    (debug-mode entitlement extension process path file-mode vnode-type device)
    (send-signal report no-report deprecated rootless)
    #f
    24 21 7 0)
  (file-write-mode
    (debug-mode entitlement extension process path file-mode vnode-type device)
    (send-signal report no-report deprecated rootless)
    #f
    25 21 7 0)
  (file-write-owner
    (debug-mode entitlement extension process path file-mode vnode-type device)
    (send-signal report no-report deprecated rootless)
    #f
    26 21 7 0)
  (file-write-setugid
    (debug-mode entitlement extension process path file-mode vnode-type device)
    (send-signal report no-report deprecated rootless)
    #f
    27 21 7 0)
  (file-write-times
    (debug-mode entitlement extension process path file-mode vnode-type device)
    (send-signal report no-report deprecated rootless)
    #f
    28 21 7 0)
  (file-write-unlink
    (debug-mode entitlement extension process path file-mode vnode-type device)
    (send-signal report no-report deprecated rootless)
    #f
    29 21 7 0)
  (file-write-xattr
    (debug-mode entitlement extension process path file-mode vnode-type device xattr)
    (send-signal report no-report deprecated rootless)
    #f
    30 21 7 0)
  (generic-issue-extension
    (debug-mode entitlement extension process extension-class)
    (send-signal report no-report deprecated rootless)
    #f
    31 0)
  (qtn-user
    (debug-mode entitlement extension process path)
    (send-signal report no-report deprecated rootless)
    #f
    32 0)
  (qtn-download
    (debug-mode entitlement extension process path)
    (send-signal report no-report deprecated rootless)
    #f
    33 0)
  (qtn-sandbox
    (debug-mode entitlement extension process path)
    (send-signal report no-report deprecated rootless)
    #f
    34 0)
  (hid-control
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    35 0)
  (iokit*
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    36 0)
  (iokit-issue-extension
    (debug-mode entitlement extension process extension-class)
    (send-signal report no-report deprecated rootless)
    #f
    37 36 0)
  (iokit-open
    (debug-mode entitlement extension process iokit-user-client iokit-connection)
    (send-signal report no-report deprecated rootless)
    #f
    38 36 0)
  (iokit-set-properties
    (debug-mode entitlement extension process iokit-property iokit-user-client iokit-connection)
    (send-signal report no-report deprecated rootless)
    #f
    39 36 0)
  (iokit-get-properties
    (debug-mode entitlement extension process iokit-property iokit-user-client iokit-connection)
    (send-signal report no-report deprecated rootless)
    allow
    40 36 0)
  (ipc*
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    41 0)
  (ipc-posix*
    (debug-mode entitlement extension process posix-ipc)
    (send-signal report no-report deprecated rootless)
    #f
    42 41 0)
  (ipc-posix-issue-extension
    (debug-mode entitlement extension process posix-ipc extension-class)
    (send-signal report no-report deprecated rootless)
    #f
    43 42 41 0)
  (ipc-posix-sem
    (debug-mode entitlement extension process posix-ipc semaphore)
    (send-signal report no-report deprecated rootless)
    #f
    44 42 41 0)
  (ipc-posix-shm*
    (debug-mode entitlement extension process posix-ipc)
    (send-signal report no-report deprecated rootless)
    #f
    45 42 41 0)
  (ipc-posix-shm-read*
    (debug-mode entitlement extension process posix-ipc)
    (send-signal report no-report deprecated rootless)
    #f
    46 45 42 41 0)
  (ipc-posix-shm-read-data
    (debug-mode entitlement extension process posix-ipc)
    (send-signal report no-report deprecated rootless)
    #f
    47 46 45 42 41 0)
  (ipc-posix-shm-read-metadata
    (debug-mode entitlement extension process posix-ipc)
    (send-signal report no-report deprecated rootless)
    #f
    48 46 45 42 41 0)
  (ipc-posix-shm-write*
    (debug-mode entitlement extension process posix-ipc)
    (send-signal report no-report deprecated rootless)
    #f
    49 45 42 41 0)
  (ipc-posix-shm-write-create
    (debug-mode entitlement extension process posix-ipc)
    (send-signal report no-report deprecated rootless)
    #f
    50 49 45 42 41 0)
  (ipc-posix-shm-write-data
    (debug-mode entitlement extension process posix-ipc)
    (send-signal report no-report deprecated rootless)
    #f
    51 49 45 42 41 0)
  (ipc-posix-shm-write-unlink
    (debug-mode entitlement extension process posix-ipc)
    (send-signal report no-report deprecated rootless)
    #f
    52 49 45 42 41 0)
  (ipc-sysv*
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    53 41 0)
  (ipc-sysv-msg
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    54 53 41 0)
  (ipc-sysv-sem
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    55 53 41 0)
  (ipc-sysv-shm
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    56 53 41 0)
  (job-creation
    (debug-mode entitlement extension process path)
    (send-signal report no-report deprecated rootless)
    deny
    57 0)
  (load-unsigned-code
    (debug-mode entitlement extension process path)
    (send-signal report no-report deprecated rootless)
    #f
    58 0)
  (lsopen
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    59 0)
  (mach*
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    60 0)
  (mach-bootstrap
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    61 60 0)
  (mach-issue-extension
    (debug-mode entitlement extension process mach extension-class)
    (send-signal report no-report deprecated rootless)
    #f
    62 60 0)
  (mach-lookup
    (debug-mode entitlement extension process mach)
    (send-signal report no-report deprecated rootless)
    #f
    63 60 0)
  (mach-per-user-lookup
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    64 60 0)
  (mach-priv*
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    65 60 0)
  (mach-priv-host-port
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    66 65 60 0)
  (mach-priv-task-port
    (debug-mode entitlement extension process path)
    (send-signal report no-report deprecated rootless)
    #f
    67 65 60 0)
  (mach-register
    (debug-mode entitlement extension process mach)
    (send-signal report no-report deprecated rootless)
    #f
    68 60 0)
  (mach-task-name
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    69 60 0)
  (network*
    (debug-mode entitlement extension process socket network path file-mode vnode-type)
    (send-signal report no-report deprecated rootless)
    #f
    70 0)
  (network-inbound
    (debug-mode entitlement extension process socket network path file-mode vnode-type)
    (send-signal report no-report deprecated rootless)
    #f
    71 70 0)
  (network-bind
    (debug-mode entitlement extension process socket network path file-mode vnode-type)
    (send-signal report no-report deprecated rootless)
    #f
    72 71 70 0)
  (network-outbound
    (debug-mode entitlement extension process socket network path file-mode vnode-type)
    (send-signal report no-report deprecated rootless)
    #f
    73 70 0)
  (user-preference*
    (debug-mode entitlement extension process preference-domain)
    (send-signal report no-report deprecated rootless)
    #f
    74 0)
  (user-preference-read
    (debug-mode entitlement extension process preference-domain)
    (send-signal report no-report deprecated rootless)
    #f
    75 74 0)
  (user-preference-write
    (debug-mode entitlement extension process preference-domain)
    (send-signal report no-report deprecated rootless)
    #f
    76 74 0)
  (process*
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    77 0)
  (process-exec*
    (debug-mode entitlement extension process path file-mode)
    (send-signal report no-report deprecated rootless no-sandbox)
    #f
    78 77 0)
  (process-exec-interpreter
    (debug-mode entitlement extension process path file-mode)
    (send-signal report no-report deprecated rootless no-sandbox)
    #f
    79 78 77 0)
  (process-fork
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    80 77 0)
  (process-info*
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    allow
    81 77 0)
  (process-info-listpids
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    82 81 77 0)
  (process-info-pidinfo
    (debug-mode entitlement extension process target)
    (send-signal report no-report deprecated rootless)
    #f
    83 81 77 0)
  (process-info-pidfdinfo
    (debug-mode entitlement extension process target)
    (send-signal report no-report deprecated rootless)
    #f
    84 81 77 0)
  (process-info-pidfileportinfo
    (debug-mode entitlement extension process target)
    (send-signal report no-report deprecated rootless)
    #f
    85 81 77 0)
  (process-info-setcontrol
    (debug-mode entitlement extension process target)
    (send-signal report no-report deprecated rootless)
    #f
    86 81 77 0)
  (process-info-dirtycontrol
    (debug-mode entitlement extension process target)
    (send-signal report no-report deprecated rootless)
    #f
    87 81 77 0)
  (process-info-rusage
    (debug-mode entitlement extension process target)
    (send-signal report no-report deprecated rootless)
    #f
    88 81 77 0)
  (pseudo-tty
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    89 0)
  (signal
    (debug-mode entitlement extension process target)
    (send-signal report no-report deprecated rootless)
    #f
    90 0)
  (sysctl*
    (debug-mode entitlement extension process sysctl)
    (send-signal report no-report deprecated rootless)
    #f
    91 0)
  (sysctl-read
    (debug-mode entitlement extension process sysctl)
    (send-signal report no-report deprecated rootless)
    #f
    92 91 0)
  (sysctl-write
    (debug-mode entitlement extension process sysctl)
    (send-signal report no-report deprecated rootless)
    #f
    93 91 0)
  (system*
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    94 0)
  (system-acct
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    95 94 0)
  (system-audit
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    96 94 0)
  (system-chud
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    97 94 0)
  (system-debug
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    98 94 0)
  (system-fsctl
    (debug-mode entitlement extension process fsctl)
    (send-signal report no-report deprecated rootless)
    #f
    99 94 0)
  (system-info
    (debug-mode entitlement extension process info)
    (send-signal report no-report deprecated rootless)
    #f
    100 94 0)
  (system-kext*
    (debug-mode entitlement extension process kext-bundle-id)
    (send-signal report no-report deprecated rootless)
    #f
    101 94 0)
  (system-kext-load
    (debug-mode entitlement extension process kext-bundle-id)
    (send-signal report no-report deprecated rootless)
    #f
    102 101 94 0)
  (system-kext-unload
    (debug-mode entitlement extension process kext-bundle-id)
    (send-signal report no-report deprecated rootless)
    #f
    103 101 94 0)
  (system-lcid
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    104 94 0)
  (system-mac-label
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    105 94 0)
  (system-nfssvc
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    106 94 0)
  (system-privilege
    (debug-mode entitlement extension process priv)
    (send-signal report no-report deprecated rootless grant)
    allow
    107 94 0)
  (system-reboot
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    108 94 0)
  (system-sched
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    109 94 0)
  (system-set-time
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    110 94 0)
  (system-socket
    (debug-mode entitlement extension process socket)
    (send-signal report no-report deprecated rootless)
    #f
    111 94 0)
  (system-suspend-resume
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    112 94 0)
  (system-swap
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    113 94 0)
  (system-write-bootstrap
    (debug-mode entitlement extension process)
    (send-signal report no-report deprecated rootless)
    #f
    114 94 0))

;;; Filters

;; Filters have the form (filter type merge name . args)
;; e.g. (filter path 0 literal "foo")
(define %f/type cadr)                   ; filter type
(define %f/merge caddr)                 ; filter merge argument
(define %f/name cadddr)                 ; filter name
(define %f/args cddddr)                 ; filter arguments

;; The %filter function produces a generic filter from its name, type, the
;; number of unrepeatable arguments to the filter, whether the filter can be
;; merged with similar filters given its arguments, a function that checks the
;; filter's arguments, and a function that processes the filter's arguments.
(define (%filter name type limit merge test process)
  (lambda args
    (if (test args)
        (let ((processed (process args))
              (merge-count (and merge limit)))
          (if (and (not merge)
                   (> (length processed)
                      (+ limit 1)))
              (apply require-any
                     (map (lambda (arg)
                            `(filter ,type
                                     ,merge-count
                                     ,name
                                     ,@(take processed limit)
                                     ,arg))
                          (drop processed limit)))
              `(filter ,type ,merge-count ,name . ,processed)))
        (error (string-append
                "malformed "
                (symbol->string type)
                " filter")))))

;; Define a pair of standard string filters given a type, name, and the
;; identifiers for the literal and regex versions of the filter.
(macro (%string-filter form)
  (let* ((ps (cdr form))
         (type (car ps))
         (name (cadr ps))
         (literal-id (caddr ps))
         (regex-id (cadddr ps))
         (test (lambda (args)
                 (and (<= 1 (length args))
                      (%every string? args))))
         (process (lambda (class)
                    (lambda (arg)
                      (cons class arg)))))
    `(begin
       ,(if literal-id
            `(define ,literal-id ,(%filter name type 1 #f test (process 'literal))))
       ,(if regex-id
            `(define ,regex-id ,(%filter name type 1 #t test (process 'regex)))))))

;; The %every predicate checks whether every element in a list satisfies a
;; predicate.  Useful for functions that check filter arguments.
(define (%every pred ls)
  (cond
   ((null? ls)
    #t)
   ((pred (car ls))
    (%every pred (cdr ls)))
   (else
    #f)))

;; The %id function is the identity function.  Useful for functions that
;; process filter arguments.
(define (%id x)
  x)

;; Define combination filters.
(define (%combination-filter name merges)
  (define (create left right)
    (list 'filter 'combination #f name left right))
  ;; Return elements of list b that aren't in list a.
  (define (set-subtract a b)
    (cond ((null? a) '())
          ((member (car a) b) (set-subtract (cdr a) b))
          (else (cons (car a) (set-subtract (cdr a) b)))))
  ;; Merge two filters.
  (define (merge left right)
    (cond
     ((and merges
           (%f/merge left)
           (%f/merge right)
           (eq? (%f/name left)
                (%f/name right)))
      ;; Merge the filters directly by combining their argument lists.
      (append left (set-subtract (drop (%f/args right)
                                       (%f/merge right))
                                 left)))
     ((eq? name (%f/name right))
      ;; Recursively merge with an existing combination filter.
      (let ((right-left (car (%f/args right)))
            (right-right (cadr (%f/args right))))
        (if (and (%f/merge right-left)
                 (eq? (%f/name left)
                      (%f/name right-left)))
            (create (merge left right-left)
                    right-right)
            (create right-left (merge left right-right)))))
     (else
      ;; Merge the filters by creating a new combination filter.
      (create left right))))
  (lambda args
    (let combine ((rest args))
      (cond
       ;; Verify that the filter has at least one argument.
       ((null? rest)
        (error (string-append
                (symbol->string name)
                " requires at least one filter")))
       ;; Verify that the arguments are filters.
       ((not (and (pair? (car rest))
                  (eq? 'filter (caar rest))))
        (error (string-append
                "arguments to "
                (symbol->string name)
                " must be filters")))
       ;; If there's only one argument, return it directly.
       ((null? (cdr rest))
        (car rest))
       ;; Expand trees of combination filters to the right.
       ;; This is not necessary, but it allows better merging of filters.
       ((eq? name (%f/name (car rest)))
        (combine (list (car (%f/args (car rest)))
                       (combine (cons (cadr (%f/args (car rest)))
                                      (cdr rest))))))
       ;; Recursively merge pairs of arguments.
       (else
        (merge (car rest)
               (combine (cdr rest))))))))
(define require-all (%combination-filter 'require-all #f))
(define require-any (%combination-filter 'require-any #t))
(define require-not
  (%filter 'require-not
           'combination
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           (lambda (args)               ; one filter argument
             (and (= 1 (length args))
                  (pair? (car args))
                  (eq? 'filter (caar args))))
           %id))                        ; no argument processing

;; Define entitlement filters.
(define %entitlement-load
  (%filter 'entitlement-load
           'entitlement
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           (lambda (args)               ; one or more string arguments
             (and (= 1 (length args))
                  (%every string? args)))
           %id))

(%string-filter entitlement
                entitlement-compare-string
                %entitlement-string
                %entitlement-regex)

(define %entitlement-boolean
  (%filter 'entitlement-compare-bool
           'entitlement
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           (lambda (args)               ; zero or one boolean argument
             (and (>= 1 (length args))
                  (%every boolean? args)))
           (lambda (args)               ; provide default argument
             (if (zero? (length args))
                 '(#t)
                 args))))

(define (entitlement-value arg)
  (cond ((string? arg) (%entitlement-string arg))
        ((boolean? arg) (%entitlement-boolean arg))
        (else (error "entitlement-value argument must be string or boolean"))))

(define (entitlement-value-regex arg)
  (cond ((string? arg) (%entitlement-regex arg))
        (else (error "entitlement-value-regex argument must be string"))))

(define (require-entitlement entitlement-name . args)
  (let ((numargs (length args)))
    (cond ((zero? numargs) (require-entitlement entitlement-name (%entitlement-boolean #t)))
          ((= 1 numargs)
            (let ((value-filter (car args)))
              (if (and (pair? value-filter)
                       (eq? 'filter (car value-filter)))
                (list 'filter 'combination #f 'require-entitlement 
                  (%entitlement-load entitlement-name)
                  value-filter)
                (error "expected filter as second argument"))))
          (else (error "too many arguments to require-entitlement")))))

;; Define path filters.
(%string-filter path
                path
                literal
                regex)
(define subpath
  (%filter 'path
           'path
           1                            ; one unrepeatable argument
           #f                           ; arguments can not be merged
           (lambda (args)               ; one or more string arguments
             (and (<= 1 (length args))
                  (%every string? args)
                  (%every (lambda (arg)
                            (let ((len (string-length arg)))
                              (if (and (< 1 len)
                                       (eqv? #\/ (string-ref arg (- len 1))))
                                  (error "subpaths must not end with a slash"))
                              (< 0 len)))
                          args)))
           (lambda (args)
             (cons 'subpath args))))
(%string-filter path
                mount-relative-path
                mount-relative-literal
                mount-relative-regex)
(define rootless-file-filter
  (%filter 'rootless-file
           'path
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           null?                        ; no arguments
           %id))                        ; no argument processing
(define rootless-mach-filter
  (%filter 'rootless-mach
           'path
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           null?                        ; no arguments
           %id))                        ; no argument processing

;; Define xattr filter.
(%string-filter xattr
                xattr
                xattr
                xattr-regex)

;; Define file-mode filter.
(define file-mode
  (%filter 'file-mode
           'file-mode
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           (lambda (args)               ; one or more mode arguments
             (and (<= 1 (length args))
                  (%every (lambda (x)
                            (and (integer? x)
                                 (<= 0 x #o7777)))
                          args)))
           %id))                        ; no argument processing

;; Define extension filters.
(define extension
  (%filter 'extension
           'extension
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           (lambda (args)               ; zero or more string arguments
             (%every string? args))
           (lambda (args)               ; provide a default argument
             (if (zero? (length args))
                 '("com.apple.app-sandbox.read-write")
                 args))))
(%string-filter extension-class
                extension-class
                extension-class
                extension-class-regex)

;; Define vnode-type filter.
(define vnode-type
  (%filter 'vnode-type
           'vnode-type
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           (lambda (args)               ; one or more vnode type arguments
             (and (<= 1 (length args))
                  (%every (lambda (x)
                            (memq x '(REGULAR-FILE DIRECTORY BLOCK-DEVICE CHARACTER-DEVICE SYMLINK SOCKET FIFO TTY)))
                          args)))
           %id))                        ; no argument processing
(define REGULAR-FILE 'REGULAR-FILE) (define DIRECTORY        'DIRECTORY       )
(define BLOCK-DEVICE 'BLOCK-DEVICE) (define CHARACTER-DEVICE 'CHARACTER-DEVICE)
(define SYMLINK      'SYMLINK     ) (define SOCKET           'SOCKET          )
(define FIFO         'FIFO        ) (define TTY              'TTY             )

;; Define debug-mode filter.
(define debug-mode
  (%filter 'debug-mode
           'debug-mode
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           null?                        ; no arguments
           %id))                        ; no argument processing

;; Define POSIX IPC filters.
(%string-filter posix-ipc
                ipc-posix-name
                ipc-posix-name
                ipc-posix-name-regex)

;; Define Mach filters.
(%string-filter mach
                global-name
                global-name
                global-name-regex)
(%string-filter mach
                local-name
                local-name
                local-name-regex)

;; Define KEXT filters.
(%string-filter kext-bundle-id
                kext-bundle-id
                kext-bundle-id
                kext-bundle-id-regex)

;; Define network filters.
(define (%network-filter name)
  (%filter name
           'network
           1                            ; one unrepeatable argument
           #f                           ; arguments can not be merged
           (lambda (args)               ; one protocol argument followed by
             (and (<= 1 (length args))  ; zero or more string arguments
                  (memq (car args)
                        '(ip ip4 ip6 tcp tcp4 tcp6 udp udp4 udp6))
                  (%every string? (cdr args))))
           (lambda (args)               ; provide a default string argument
             (if (= 1 (length args))
                 (append args '("*:*"))
                 args))))
(define (%network-legacy-filter network-filter)
  (lambda args
    (if (and (<= 1 (length args))
             (eq? 'unix (car args)))
        (if (pair? (cdr args))
            (apply regex (cdr args))
            (regex ""))
        (apply network-filter args))))
(define local  (%network-legacy-filter (%network-filter 'local )))
(define remote (%network-legacy-filter (%network-filter 'remote)))
(define unix 'unix)
(define ip   'ip  ) (define ip4  'ip4 ) (define ip6  'ip6 )
(define tcp  'tcp ) (define tcp4 'tcp4) (define tcp6 'tcp6)
(define udp  'udp ) (define udp4 'udp4) (define udp6 'udp6)
(%string-filter network control-name control-name control-name-regex)

;; Define socket filters.
(define socket-domain
  (%filter 'socket-domain
           'socket
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           (lambda (args)               ; one or more numeric arguments
             (and (<= 1 (length args))
                  (%every (lambda (arg)
                            (and (integer? arg)
                                 (< -1 arg AF_MAX)))
                          args)))
           %id))                        ; no argument processing
(define socket-type
  (%filter 'socket-type
           'socket
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           (lambda (args)               ; one or more numeric arguments
             (and (<= 1 (length args))
                  (%every integer? args)))
           %id))                        ; no argument processing
(define socket-protocol
  (%filter 'socket-protocol
           'socket
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           (lambda (args)               ; one or more numeric arguments
             (and (<= 1 (length args))
                  (%every integer? args)))
           %id))                        ; no argument processing

;; Define process target filters.
(define target
  (%filter 'target
           'target
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           (lambda (args)               ; one or more target arguments
             (and (<= 1 (length args))
                  (%every (lambda (arg)
                            (memq arg '(self pgrp others children same-sandbox)))
                          args)))
           %id))                        ; no argument processing
(define self     'self)
(define pgrp     'pgrp)
(define others   'others)
(define children 'children)
(define same-sandbox 'same-sandbox)

;; Define semaphore filters.
(define semaphore-owner
  (%filter 'semaphore-owner
           'semaphore
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           (lambda (args)               ; one or more target arguments
             (and (<= 1 (length args))
                  (%every (lambda (arg)
                            (memq arg '(self pgrp others children same-sandbox)))
                          args)))
           %id))                        ; no argument processing

;; Define fsctl filters.
(define fsctl-command
  (%filter 'fsctl-command
           'fsctl
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           (lambda (args)               ; one or more numeric arguments
             (and (<= 1 (length args))
                  (%every integer? args)))
           %id))                        ; no argument processing

;; Define ioctl filters.
(define ioctl-command
  (%filter 'ioctl-command
           'ioctl
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           (lambda (args)               ; one or more numeric arguments
             (and (<= 1 (length args))
                  (%every integer? args)))
           %id))                        ; no argument processing

;; Helper function for composing fsctl / ioctl commands.
(define (_IO g n)
   (+ n (* 256 (char->integer (car (string->list g))))))

;; Define I/O Kit filters.
(%string-filter iokit-user-client
                iokit-user-client-class
                iokit-user-client-class
                iokit-user-client-class-regex)
(%string-filter iokit-property
                iokit-property
                iokit-property
                iokit-property-regex)
(%string-filter iokit-connection
                iokit-connection
                iokit-connection
                iokit-connection-regex)

;; Define device filters.
(define device-major
  (%filter 'device-major
           'device
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           (lambda (args)               ; one or more numeric arguments
             (and (<= 1 (length args))
                  (%every integer? args)))
           %id))                        ; no argument processing
(define device-minor
  (%filter 'device-minor
           'device
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           (lambda (args)               ; one or more numeric arguments
             (and (<= 1 (length args))
                  (%every integer? args)))
           %id))                        ; no argument processing
(define device-conforms-to
  (%filter 'device-conforms-to
           'device
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           (lambda (args)               ; one or more string arguments
             (and (= 1 (length args))
                  (%every string? args)))
           %id))
                

;; Define Apple Event filters.
(%string-filter ae-destination
                appleevent-destination
                appleevent-destination
                appleevent-destination-regex)

;; Define Authorization Services right filters.
(%string-filter auth-right-name
                right-name
                right-name
                right-name-regex)

;; Define Preference filters.
(%string-filter preference-domain
                preference-domain
                preference-domain
                preference-domain-regex)

;; Define Info filters.
(%string-filter info
                info-type
                info-type
                #f)

;; Define notification filters.
(%string-filter notification
                notification-name
                notification-name
                notification-name-regex)
(define notification-payload
  (%filter 'notification-payload
           'notification
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           null?                        ; no arguments
           %id))                        ; no argument processing

;; Define privilege filters.
(define privilege-id
  (%filter 'privilege-id
           'priv
           0                            ; no unrepeatable arguments
           #f                           ; arguments can not be merged
           (lambda (args)               ; one or more numeric arguments
             (and (<= 1 (length args))
                  (%every integer? args)))
           %id))                        ; no argument processing

;; Define sysctl filters.
(%string-filter sysctl
                sysctl-name
                sysctl-name
                sysctl-name-regex)

;; Define Process filters.
(%string-filter process
                process-name
                process-name
                process-name-regex)

(define process-is-plugin
  (%filter 'process-attribute
           'process
           1                            ; one unrepeatable arguments
           #f                           ; arguments can not be merged
           null?                        ; no caller-supplied arguments
           (lambda (args)
             (cons 'is-plugin args))))

;;; Modifiers

;; Modifiers have the form (modifier check name . args)
;; e.g. (modifier #<CLOSURE> send-signal 17)
(define %m/check cadr)                  ; predicate for action compatibility
(define %m/name caddr)                  ; modifier name
(define %m/args cdddr)                  ; modifier arguments

;; Define modifiers.
(define send-signal
  (list 'send-signal
        (lambda args                    ; one integral argument, legal signal
          (and (= 1 (length args))
               (integer? (car args))
               (< 0
                  (car args)
                  __DARWIN_NSIG)))
        (lambda (rule)                  ; applies to all actions
          #t)))
(define grant
  (list 'grant
        (lambda args                    ; no arguments
          (= 0 (length args)))
        (lambda (rule)                  ; only applies to allow
          (eq? rule 'allow))))
(define report
  (list 'report
        (lambda args                    ; no arguments
          (= 0 (length args)))
        (lambda (rule)                  ; only applies to allow
          (eq? rule 'allow))))
(define no-report
  (list 'no-report
        (lambda args                    ; no arguments
          (= 0 (length args)))
        (lambda (rule)                  ; only applies to deny
          (eq? rule 'deny))))
(define no-sandbox
  (list 'no-sandbox
        (lambda args                    ; no arguments
          (= 0 (length args)))
        (lambda (rule)                  ; only applies to allow
          (eq? rule 'allow))))
(define no-callout
  (list 'deprecated
        (lambda args                    ; no arguments
          (disable-callouts)            ; superseded by sandbox option
          (= 0 (length args)))
        (lambda (rule)                  ; applies to all actions
          #t)))
(define partial-symbolication
  (list 'deprecated
        (lambda args                    ; no arguments
          (disable-full-symbolication)  ; superseded by sandbox option
          (= 0 (length args)))
        (lambda (rule)                  ; applies to all actions
          #t)))
(define rootless-modifier
  (list 'rootless
        (lambda args                    ; no arguments
          (= 0 (length args)))
        (lambda (rule)                  ; applies to deny
          (eq? rule 'deny))))

;; The with function creates a modifier.
(define (with modifier . args)
  ;; Verify that the first argument is modifier.
  (if (and (list? modifier)
           (= 3 (length modifier))
           (symbol? (car modifier))
           (procedure? (cadr modifier))
           (procedure? (caddr modifier)))
      ;; Check the modifier's arguments.
      (if (apply (cadr modifier)
                 args)
          ;; Create the modifier.
          `(modifier ,(caddr modifier)
                     ,(car modifier)
                     .
                     ,args)
          (error "malformed modifier:" (car modifier)))
      (error "illegal modifier")))

;;; Utilities

;; The %opt-remove-filters optimization removes filtered rules that have no
;; effect on the result of profile evaluation.
(define (%opt-remove-filters)
  ;; Given a rule, if it's unfiltered return its action and modifiers, if it's
  ;; filtered return #f, and if it's a jump to another operation, operate
  ;; recursively on the first rule for the target operation.
  (define (action rule)
    (case (car rule)
      ((#t) (cdr rule))
      ((#f) (action (car (vector-ref *rules* (cdr rule)))))
      (else #f)))
  ;; Iterate over every operation in the rules table from most general to least
  ;; general, because unnecessary rules for more general operations would
  ;; prevent the detection of unnecessary rules for less general operations.
  (do ((op 0 (+ op 1))
       (count (vector-length *rules*)
              count))
      ((= op count))
    ;; If the operation has at least one filtered rule and the result of
    ;; matching the last filtered rule is the same as the result of matching
    ;; none of the filtered rules, remove the last filtered rule.  There's no
    ;; need to look at more than just the last filtered rule, because
    ;; sequential rules with the same result have already been combined, and
    ;; therefore if the last filtered rule could be eliminated, the one before
    ;; it cannot (as it has a different result); and if a rule could not be
    ;; eliminated, neither can the one before it (as it determines whether the
    ;; later rules will be evaluated).
    (let ((rules (vector-ref *rules* op)))
      (if (< 1 (length rules))
          (let ((reverse-rules (reverse rules)))
            (if (equal? (cdadr reverse-rules)
                        (action (car reverse-rules)))
                (vector-set! *rules*
                             op
                             (reverse (cons (car reverse-rules)
                                            (cddr reverse-rules))))))))))

;; The %opt-remove-duplicates removes duplicate rules in chained
;; combination filters.
(define (%opt-remove-duplicates)

  ;; If given a combination filter, returns its type
  ;; (e.g. require-any), else returns #f.
  (define (which-combination filter)
    (if (and (list? filter)
             (equal? (car filter) 'filter)
             (equal? (cadr filter) 'combination)
             (member (cadddr filter) '(require-any require-all)))

        (if (not (= (length filter) 6))
            (error
             (string-append "combination filter of type '" (cadddr filter)
                            "' has wrong length "
                            (number->string (length filter))))

            (cadddr filter))

        #f))

  (define (remove-dups filter)
    ;; remove-dups-progress postorder-traverses the filter tree as
    ;; long as it encounters combination filter nodes of the given
    ;; type given in 'type' (e.g. require-any). Whenever it encounters
    ;; a combination filter node of a differing type, it isolatedly
    ;; process that subtree with an fresh, initially empty seen-list.
    ;; Nodes which aren't combination filters at all are just kept
    ;; unchanged or removed entirely if already seen in the current
    ;; remove-dups-progress context.
    (define (remove-dups-progress filter type seen)
      (let ((this-type (which-combination filter)))

        (if (equal? this-type type)
            ;; If called on a filter with the currently processed type,
            ;; dedupe the children and inspect the result.
            (let* ((left-pair (remove-dups-progress
                               (car (cddddr filter)) type seen))
                   (left (car left-pair))
                   (left-seen (cdr left-pair))

                   (right-pair (remove-dups-progress
                                (car (cdr (cddddr filter))) type left-seen))
                   (right (car right-pair))
                   (right-seen (cdr right-pair)))

              (cond
               ;; If one of the two children was completely reduced away
               ;; by deduplication, do not recreate a combination
               ;; filter, but just use the other child as new filter
               ;; node.
               ;; Note that if both children were reduced away, this
               ;; results in the current node also being empty.
               ((null? left) (cons right right-seen))
               ((null? right) (cons left right-seen))

               ;; Otherwise, recreate a combination filter node using the
               ;; two deduped children.
               (else (cons (list 'filter 'combination (caddr filter) type left right)
                           right-seen))))

            ;; If called on a filter which is either not a combination
            ;; filter of the currently processed type ...
            (begin
              (let ((filter' (if this-type
                                 ;; ... either isolatedly dedupe the
                                 ;; subtree if it is a combination
                                 ;; filter of another type ...
                                 (car (remove-dups-progress filter
                                                            this-type ()))
                                 ;; .. or use it directly if it
                                 ;; is not a combination filter at all.
                                 filter)))
                ;; This is the actual deduping: the inspected filter is
                ;; reduced away if it was already seen, or kept and
                ;; added to the seen list if not.
                (if (member filter' seen)
                    (cons () seen)
                    (cons filter' (cons filter' seen))))))))

    ;; We start on the filter's root node with the never occurring
    ;; type (). If the root node is indeed a combination filter,
    ;; remove-dups-progress will restart itself with its type.
    (car (remove-dups-progress filter () ())))

  (do ((op 0 (+ op 1))
       (count (vector-length *rules*)
              count))
      ((= op count))
    (vector-set! *rules* op
                 (map (lambda (rule)
                        (cons (remove-dups (car rule)) (cdr rule)))
                      (vector-ref *rules* op)))))

;; The %record function converts the evaluated rules back into an SBPL-like
;; format to aid in debugging complicated sandbox profiles.
(define (%record)
  ;; Remove unnecessary information from the filters before printing.
  (define (process-filter f)
    (if (eq? 'combination (%f/type f))
        (cons (%f/name f)
              (map process-filter (%f/args f)))
        (cons (%f/name f)
              (%f/args f))))
  ;; Iterate over the rules for each operation.
  (do ((op 0 (+ op 1))
       (count (vector-length *rules*)
              count))
      ((= op count))
    (do ((rules (reverse (vector-ref *rules* op))
                (cdr rules)))
        ((null? rules))
      (if (caar rules)
          (let ((action (cadar rules))
                (modifiers (cddar rules)))
            (let write-rule ((filters (caar rules)))
              (if (and (pair? filters)
                       (eq? 'require-any (%f/name filters)))
                  ;; For rules that were combined with a require-any
                  ;; filter, recursively display them as separate rules.
                  (begin
                    (write-rule (cadr (%f/args filters)))
                    (write-rule (car (%f/args filters))))
                  ;; Write other rules out in a format resembling SBPL.
                  (begin
                    (write (append (list action (%o/name (vector-ref *operations* op)))
                                   (if (eq? #t filters)
                                       (list)
                                       (list (process-filter filters)))
                                   (map (lambda (m)
                                          (cons 'with (cons (%m/name m)
                                                            (%m/args m))))
                                        modifiers)))
                    (newline)))))))))

(define (%emit-implicit-rules)
  ;; Determine if an operation can ever return a certain action.
  (define (returns? op action)
    (let scan ((rules (vector-ref *rules* (%o/code op))))
      (cond
       ((not (caar rules))
        (scan (vector-ref *rules* (cdar rules))))
       ((eq? action (cadar rules))
        #t)
       ((pair? (cdr rules))
        (scan (cdr rules)))
       (else
        #f))))
  (define (allowed? op)
    (returns? op 'allow))
  (define (denied? op)
    (returns? op 'deny))
  ;; Allow mach-bootstrap if mach-lookup is ever allowed.
  (if (or *trace* (allowed? mach-lookup))
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
  ;; Never allow a sandboxed process to access sandbox cache directories.
  (let ((count (vector-length *operations*)))
    (do ((op 0 (+ op 1)))
        ((= op count))
      (let ((operation (vector-ref *operations* op)))
        (if (and (memq 'path (%o/filters operation))
                 (not (memq 'cache-safe (%o/filters operation))))
            (deny operation (regex #"/com\.apple\.sandbox($|/)"))))))
  ;; Always allow a process to signal itself.
  (allow signal (target self)))

;; The %finalize function is called after a profile has been evaluated.
(set! %finalize
      (lambda ()
        (if (not (param "NO_IMPLICIT_RULES")) (%emit-implicit-rules))
        ;; Optimize the profile rules.
        (%opt-remove-filters)
        (if *eliminate-duplicate-rules* (%opt-remove-duplicates))
        ;; Dump the evaluated profile.
        (if *record* (with-output-to-file *record* %record))))

;;; Aliases

(macro (debug args))
(define getenv param)
(define file-fsctl system-fsctl)
(define ipc-posix-shm ipc-posix-shm*)
(define sysctl-write sysctl*)
(define system-misc system*)
(define time-set system-set-time)
(define from local)
(define to remote)
(define unix-socket unix)
(define no-profile no-sandbox)
(define no-log no-report)
(define granted-extensions extension)
(define (container) (extension *ios-sandbox-container*))
(define (executable-bundle) (extension *ios-sandbox-executable*))
(define (application-group) (extension *ios-sandbox-application-group*))
(define file-issue-extension* file-issue-extension)
(define file-issue-extension-read file-issue-extension)
(define file-issue-extension-write file-issue-extension)
(define file-unlink file-write-unlink)
(define mach-extension extension)
(define (tty) (vnode-type TTY))
(define file-write-mount file-mount)
(define file-write-unmount file-unmount)
(define file-write-umount file-unmount)
(define process-exec process-exec*)
