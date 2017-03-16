(in-package :cl-user)
(defpackage :daemon
  (:use :cl)
  (:export #:exit
           #:daemonize
           #:fork))
