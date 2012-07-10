(require :asdf)
#+sbcl(require :sb-posix)

(let ((base (make-pathname :name nil :type nil :defaults *load-pathname*)))
  (load (merge-pathnames "library/daemon/daemon.lisp" base)))

(in-package :daemon)

(defvar *project-directory* nil)
(defvar *run-child-function* 'run-child)

(defun _ (pkg sym &rest args)
  (let ((symbol (intern (symbol-name sym) (find-package pkg))))
    (if args
	(apply symbol args)
	symbol)))

(defun log% (message path)
  (with-open-file (*standard-output* path
				     :direction :output
				     :if-exists :append
				     :if-does-not-exist :create)
    (format t "~A" message)))

(defun ymdhms (&optional (universal-date (get-universal-time)))
  (let ((date(multiple-value-list (decode-universal-time universal-date))))
    (format nil "~D/~2,'0D/~2,'0D ~2,'0D:~2,'0D:~2,'0D" (sixth date) (fifth date) (fourth date) (third date) (second date) (first date))))

(defvar *username*
  (load-time-value
   (if (zerop (getuid))
       (third (pathname-directory *load-pathname*))
       (uid-username (getuid)))))

(defun shepherd-1process (function &key (quit-checker (constantly nil)))
  (loop 
     :with quit
     :for pid := (fork)
     :do 
     (if (zerop pid)
	 (unwind-protect
	      (funcall function)
	   (exit))
	 (loop :while (and (waitpid pid +wnohang+) (not quit))
	    :do (when (funcall quit-checker) 
		  (setf quit t))
	    (sleep 5)))
     (sleep 5)
     :until quit))

(defmacro with-shepherd-1process (options lambda-list &body body)
  `(shepherd-1process
    #'(lambda ,lambda-list ,@body)
    ,@options))

(defun delete-file-if-probed (path)
  (when (probe-file path)
    (delete-file path)))

(defun shepherd-subdirectories (function path &key 
				quitable
				(quit-file-name "quit")
				(quit-checker (constantly nil))
				stoppable
				(stop-file-name "stop")
				(restart-file-name "restart"))
  (loop 
     :with project-directory := path
     :with servers := '()
     :until (sleep 1)
     :for globally-stop := (when stoppable
			     (probe-file (merge-pathnames stop-file-name  project-directory)))
     :for should-quit := (or should-quit
			     (funcall quit-checker)
			     (when quitable 
			       (delete-file-if-probed
				(merge-pathnames quit-file-name project-directory))))
     :do 
     (loop 
	:with result := '()
	:for path :in (remove-if #'(lambda (x)
				     (eql #\. (aref(first(last (pathname-directory x))) 0)))
				 (remove-if #'pathname-name
					    (_ :cl-fad :list-directory project-directory)))
	:for last := (first (last (pathname-directory  path)))
	:for assoc := (assoc last servers :test #'equal)
	:for restarted := (when (probe-file (merge-pathnames restart-file-name path))
			    (log% (format nil "~@{~A~^,~}~%"
					  last  "module restarted"
					  (format nil "~A module restarted" last)
					  (ymdhms))
				  (merge-pathnames "log" project-directory))
			    (delete-file-if-probed (merge-pathnames restart-file-name path)))
	:for stopped := (or (probe-file (merge-pathnames stop-file-name  path))
			    restarted
			    globally-stop)
	:do
	;; invoke process and living process into variable result
	;; remove living process from servers
	;; so after exit from this room will result in variable servers only has dead processes.
	(unless stopped
	  (if assoc
	      (progn
		(setf servers (remove last servers :key #'car :test #'equal))
		(push assoc result))
	      (let ((pid (fork)))
		(when (zerop pid)
		  (unwind-protect
		       (funcall function :path path)
		    (exit)))
		(log% (format nil "~@{~A~^,~}~%"
			      (first (last (pathname-directory path)))
			      (format nil  "~A module launched" (first (last (pathname-directory path))))
			      (format nil "PID:~A" pid)
			      (ymdhms))
		      (merge-pathnames "log" project-directory))
		(sleep 1)
		(push (cons last pid)
		      result))))
	:finally 
	(progn
	  ;;kill processes for removed subdir
	  (loop :for (dir . pid) :in servers
	     :do (if (kill pid +sigkill+)
		     (progn
		       (log% (format nil "~@{~A~^,~}~%" 
				     dir
				     (format nil  "module finished~A" dir)
				     (format nil "PID:~A" pid)
				     (ymdhms))
			     (merge-pathnames "log" project-directory))
		       (waitpid pid 0))
		     :error-sigkill))
	  ;;check living processes
	  (setf servers
		(loop :for elt :in result
		   :for (dir . pid) := elt
		   :for r := (if (waitpid pid +wnohang+)
				 t
				 (log% (format t "~@{~S ~}~%"
					       :halted (multiple-value-list (decode-universal-time (get-universal-time)))
					       pid  dir)
				       (merge-pathnames "log" project-directory)))
		   :when r
		   :collect elt))
	  (when should-quit 
	    (return-from shepherd-subdirectories))))))

(defmacro with-shepherd-subdirectories (options lambda-list &body body)
  `(shepherd-subdirectories #'(lambda ,lambda-list ,@body) 
			    ,@options))

(defun drop-previlege (user)
  (let ((pswd (getpwnam user)))
    #+linux ;; oom_adj should be 0
    (when (zerop (getuid))
      (with-open-file (out (format nil "/proc/~A/oom_adj" (getpid))
			   :direction :output
			   :if-exists :overwrite
			   :if-does-not-exist :create)
	(format out "0")))
    (setuid (passwd-uid pswd))))

(defun run-child (&key path)
  (ignore-errors 
    (let* ((lpath (merge-pathnames "loadup.lisp" path))
	   (noload (merge-pathnames "noload" path)))
      (in-package :cl-user)
      (with-open-file (*standard-output* (merge-pathnames "log" path)
					 :direction :output
					 :if-exists :append
					 :if-does-not-exist :create)
	(handler-case 
	    (progn
	      (format t "~&start ~S~%" (multiple-value-list 
				      (decode-universal-time (get-universal-time))))
	      (force-output)
	      (let* ((swank-file (merge-pathnames "swank-port" path))
		     (port (and (probe-file swank-file)
				(ignore-errors (with-open-file (in swank-file)
						 (read in))))))
		(when port
		  (setf (symbol-value (_ :swank :*coding-system*)) "utf-8-unix")
		  (_ :swank :setup-server port #'(lambda (x)
						   (setf port x))
		     (symbol-value (_ :swank :*communication-style*))
		     t (if (< (parse-integer (remove #\- (symbol-value (_ :swank :*swank-wire-protocol-version*)))) 20120106) "utf-8-unix" nil))
		  (format t "swank start at port ~S~%" port))
		(if (and (probe-file lpath)
			 (not (probe-file noload)))
		    (progn
		      (push path (symbol-value (_ :ql :*local-project-directories*)))
		      (push (merge-pathnames "../../library/" path)
			    (symbol-value (_ :ql :*local-project-directories*)))
		      (format t "~&loading ~S~%" lpath)
		      (force-output)
		      (load lpath)
		      (format t "~&load complete~%"))
		    (format t "~&skip loading ~A~%" lpath))
		(force-output))
	      (loop :while (/= (getppid) 1)
		 :do
		 (sleep 5))
	      (format t "~&parent process is dead~%"))
	  (serious-condition (x)
	    (_ :trivial-backtrace :print-backtrace
	       x :output *standard-output*)))
	(format t "~&finish ~S~%" (multiple-value-list 
				   (decode-universal-time (get-universal-time))))))))

(defmacro with-daemonize (&body body)
  `(progn
     (daemonize :exit-parent t)
     ,@body
     (exit)))

(defun run-projects (&key (project-name "lisp-services")
		     directory
		     qlpath
		     (child-function *run-child-function*)
		     initialize)
  (declare (ignorable project-name))
  (chore)
  (with-daemonize
    (with-shepherd-1process () ()
      (drop-previlege *username*)
      (setenv "HOME" (passwd-dir (getpwnam *username*)))
      (let ((ql (or qlpath 
		    (merge-pathnames "quicklisp/setup.lisp"
				     (pathname (concatenate 'string (passwd-dir (getpwnam *username*)) "/"))))))
	(when (ignore-errors (probe-file ql))
	  (load ql))
	(_ :asdf :enable-asdf-binary-locations-compatibility 
	   :centralize-lisp-binaries t
	   :default-toplevel-directory (merge-pathnames "../.fasls/" directory)
	   :map-all-source-files t))
      (_ :ql :quickload 
	 '(:trivial-backtrace :swank :cl-fad))
      (when (and initialize (functionp initialize))
	(funcall initialize))
      (shepherd-subdirectories
       child-function
       directory
       :quit-checker (lambda () (= (getppid) 1))))))

(in-package :common-lisp-user)

(daemon::run-projects 
 :directory (merge-pathnames "procs/" (make-pathname :name nil :type nil :defaults *load-pathname*))
 :qlpath (merge-pathnames "setup.lisp" <!-- TMPL_VAR qlpath -->))

(daemon:exit)
