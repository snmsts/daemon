(asdf:load-system :trivial-features :verbose nil)

(asdf:defsystem :daemon
  :version "0.0.4"
  :author "Masatoshi SANO"
  :description "Process daemonization for some common lisp."
  :licence "MIT"
  :components ((:file "package")
               #-windows(:file "daemon"))
  :serial t
  :depends-on (#+sbcl :sb-posix))
