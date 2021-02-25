(defsystem :daemon
  :version "0.0.4"
  :author "Masatoshi SANO"
  :description "Process daemonization for some common lisp."
  :licence "MIT"
  :depends-on (#:trivial-features)       
  :components ((:file "package")
               #-windows(:file "daemon"))
  :serial t
  :depends-on (#+sbcl :sb-posix))
