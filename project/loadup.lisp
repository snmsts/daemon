(require :asdf)
#+sbcl(require :sb-posix)

(let ((base (make-pathname :name nil :type nil :defaults *load-pathname*)))
  (load (merge-pathnames "library/daemon/daemon.lisp" base)))

(daemon:run-projects 
 :directory (merge-pathnames "procs/" (make-pathname :name nil :type nil :defaults *load-pathname*))
 :qlpath (merge-pathnames "setup.lisp" <!-- TMPL_VAR qlpath -->))

(daemon::exit)
