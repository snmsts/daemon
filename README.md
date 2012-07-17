# daemon

This is a library which has simple functionality to make lisp process daemonize on unix like platform.
 
## Usage
Currently only `daemon:daemonize` and `daemon:exit` function are exported.
so just a simple example is good enough to show the idea of this library.

```
(daemon:daemonize :exit-parent t)

(with-open-file (out #P "/tmp/daemonlog" :direction :output :if-exists :supersede)
  (format out "~A ~A~%~A~%"
          (lisp-implementation-type)
          (lisp-implementation-version)
          (daemon::getpid)))

(sleep 90)
(daemon:exit)
```
then you can see the effect by the command below.
```
ps axo user,pid,ppid,command
```