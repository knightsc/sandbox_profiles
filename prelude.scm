;;;;;; Sandbox Profile Language prelude

;;; SBPL library functions

;; param -- get the value of a compile-time parameter, or false if undefined
(define (param key)
  (let ((value (assoc key *params*)))
    (and value (cdr value))))

;;; System constants

(define __DARWIN_NSIG 32)
(define SIGHUP 1)
(define SIGINT 2)
(define SIGQUIT 3)
(define SIGILL 4)
(define SIGTRAP 5)
(define SIGABRT 6)
(define SIGPOLL 7)
(define SIGIOT SIGABRT)
(define SIGEMT 7)
(define SIGFPE 8)
(define SIGKILL 9)
(define SIGBUS 10)
(define SIGSEGV 11)
(define SIGSYS 12)
(define SIGPIPE 13)
(define SIGALRM 14)
(define SIGTERM 15)
(define SIGURG 16)
(define SIGSTOP 17)
(define SIGTSTP 18)
(define SIGCONT 19)
(define SIGCHLD 20)
(define SIGTTIN 21)
(define SIGTTOU 22)
(define SIGIO 23)
(define SIGXCPU 24)
(define SIGXFSZ 25)
(define SIGVTALRM 26)
(define SIGPROF 27)
(define SIGWINCH 28)
(define SIGINFO 29)
(define SIGUSR1 30)
(define SIGUSR2 31)

(define PRIV_ADJTIME 1000)
(define PRIV_PROC_UUID_POLICY 1001)
(define PRIV_GLOBAL_PROC_INFO 1002)
(define PRIV_SYSTEM_OVERRIDE 1003)
(define PRIV_HW_DEBUG_DATA 1004)
(define PRIV_SELECTIVE_FORCED_IDLE 1005)
(define PRIV_PROC_TRACE_INSPECT 1006)
(define PRIV_DARKBOOT 1007)
(define PRIV_WORK_INTERVAL 1008)
(define PRIV_VM_PRESSURE 6000)
(define PRIV_VM_JETSAM 6001)
(define PRIV_VM_FOOTPRINT_LIMIT 6002)
(define PRIV_NET_PRIVILEGED_TRAFFIC_CLASS 10000)
(define PRIV_NET_PRIVILEGED_SOCKET_DELEGATE 10001)
(define PRIV_NET_INTERFACE_CONTROL 10002)
(define PRIV_NET_PRIVILEGED_NETWORK_STATISTICS 10003)
(define PRIV_NET_PRIVILEGED_NECP_POLICIES 10004)
(define PRIV_NET_RESTRICTED_AWDL 10005)
(define PRIV_NET_PRIVILEGED_NECP_MATCH 10006)
(define PRIV_NETINET_RESERVEDPORT 11000)
(define PRIV_VFS_OPEN_BY_ID 14000)
(define PRIV_VFS_MOVE_DATA_EXTENTS 14001)
