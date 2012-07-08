(defpackage :daemon
  (:use :cl)
  (:export #:run-projects 
	   #:exit
	   #:daemonize))

(in-package :daemon)

#+allegro
(eval-when (:load-toplevel :execute)
  (unless (excl.osi:detach-from-terminal-supported-p)
    (error "not supported allegro cl to detach console")))

(defvar *project-directory* nil)

(defconstant +wnohang+ (if (boundp '+wnohang+) +wnohang+
			   #+sbcl 1
			   #+allegro 1
			   #+ccl #$WNOHANG))

(defconstant +sigkill+ (if (boundp '+sigkill+) +sigkill+
			   #+sbcl sb-unix:sigkill
			   #+allegro excl::*sigkill*
			   #+ccl #$SIGKILL))

(defvar *o-rdonly* (or 
		    #+sbcl sb-posix:o-rdonly
		    #+allegro excl::*o-rdonly*
		    #+ccl #$O_RDONLY 
		    ))

(defvar *o-wronly* (or
		    #+sbcl sb-posix:o-wronly
		    #+allegro excl::*o-wronly*
		    #+ccl #$O_WRONLY
		    ))

(defvar *o-append* (or 
		    #+sbcl sb-posix:o-append
		    #+allegro excl::*o-append*
		    #+ccl #$O_APPEND
		    ))

(defvar *run-child-function* 'run-child)

(defun chdir (dir)
  #+sbcl (sb-posix:chdir dir)
  #+allegro (excl:chdir dir)
  #+ccl (ccl::%chdir dir))

(defun setenv (env val)
  #+sbcl (sb-posix:setenv env val 1)
  #+allegro (setf (sys:getenv env) val)
  #+ccl (ccl:setenv env val t))

(defun dup2  (old new)
  #+sbcl (sb-posix:dup2 old new)
  #+allegro (excl.osi::syscall-dup2 old new)
  #+ccl (#_dup2 old new))

(defun fork ()
  (or
   #+sbcl (sb-posix:fork)
   #+allegro (excl.osi:fork)
   #+ccl (#_fork)
   (error "should support fork")))

(defun popen (fspec flags)
  #+sbcl (sb-posix:open fspec flags)
  #+allegro (excl.osi:os-open fspec flags)
  #+ccl (ccl::with-cstrs ((cstr fspec)) (#_open cstr flags)))

(defun pclose (fd)
  #+sbcl (sb-posix:close fd)
  #+allegro (excl.osi:os-close fd)
  #+ccl (#_close fd))

(defun exit ()
  #+sbcl #+#.(cl:if (cl:find-symbol (cl:symbol-name :exit) :sb-posix) '(:and) '(:or)) (sb-posix:exit 0)
         #-#.(cl:if (cl:find-symbol (cl:symbol-name :exit) :sb-posix) '(:and) '(:or)) (sb-unix:unix-exit)
  #+allegro (excl:exit 0 :quiet t)
  #+ccl (#_exit 0))

(defun waitpid (pid option)
  #+sbcl(zerop (sb-posix:waitpid pid option))
  #+allegro (not (excl.osi:waitpid pid :wnohang (not (zerop option))))
  #+ccl (zerop (#_waitpid pid 0 option)))

(defun setsid ()
  #+sbcl(not (minusp (sb-posix:setsid))) 
  #+allegro (excl.osi:setsid)
  #+ccl (not (minusp (#_setsid))))

(defun setuid (uid)
  #+sbcl(sb-posix:setuid uid)
  #+allegro (excl.osi:setuid uid)
  #+ccl (#_setuid uid))

(defun getpid ()
  #+sbcl (sb-posix:getpid)
  #+allegro (excl.osi:getpid)
  #+ccl (ccl::getpid))

(defun getppid ()
  #+sbcl (sb-posix:getppid)
  #+allegro (excl.osi:getppid)
  #+ccl (#_getppid))

(defun getuid ()
  #+sbcl (sb-posix:getuid)
  #+allegro (excl.osi:getuid)
  #+ccl (#_getuid))

(defun getpwnam (name)
  #+sbcl (sb-posix:getpwnam name)
  #+allegro (excl.osi:getpwnam name)
  #+ccl (ccl::with-cstrs ((cstr name)) (#_getpwnam cstr)))

(defun kill (pid signal)
  #+sbcl (not (= -1 (sb-posix:kill pid signal))) 
  #+allegro (excl.osi:kill pid signal)
  #+ccl (#_kill pid signal))

(defun uid-username (uid)
  #+sbcl (sb-posix:passwd-name (sb-posix:getpwuid uid))
  #+allegro (excl.osi:pwent-name (excl.osi:getpwuid uid))
  #+ccl (ccl:%get-cstring (ccl:pref (#_getpwuid uid) :passwd.pw_name)))

(defun passwd-uid (pswd)
  #+sbcl(sb-posix:passwd-uid pswd)
  #+allegro (excl.osi:pwent-uid pswd)
  #+ccl (ccl:pref pswd :passwd.pw_uid))

(defun passwd-dir (pswd)
  #+sbcl (sb-posix:passwd-dir pswd)
  #+allegro (excl.osi:pwent-dir pswd)
  #+ccl (ccl:%get-cstring(ccl:pref pswd :passwd.pw_dir)))

;; I have to change this function's name...
(defun close-fd-streams ()
  (chdir "/")
  #-allegro
  (progn 
    #+sbcl
    (progn
      (setf sb-sys:*stdin* (make-concatenated-stream))
      (when (sb-sys:fd-stream-p sb-sys:*tty*) 
	(close sb-sys:*tty* :abort t)
	(setf sb-sys:*tty* (make-two-way-stream sb-sys:*stdin* sb-sys:*stdout*))))
    #+ccl 
    (setf ccl::*stdin* (make-concatenated-stream)
	  ccl::*stdout* (make-broadcast-stream)
	  ccl::*terminal-input* ccl::*stdin*
	  ccl::*terminal-output* ccl::*stdout*
	  ccl::*terminal-io* (make-two-way-stream 
			      ccl::*terminal-input* ccl::*terminal-output*))
    (let ((write (popen "/dev/null" *o-rdonly*)))
      (dup2 write 0)
      (dup2 write 2))
    (dup2 (popen "/dev/null" (logior *o-wronly*
				     *o-append*)) 1))
  #+allegro 
  (excl.osi:detach-from-terminal))

(defun chore ()
  #+linux
  ;; oom_adj should be -17
  (when (zerop (getpid))
    (with-open-file (out (format nil "/proc/~A/oom_adj" (getpid))
			 :direction :output
			 :if-exists :overwrite
			 :if-does-not-exist :create)
      (format out "-17")))
  #+sbcl
  (setq sb-impl::*default-external-format* :utf-8))

#|
From here by should not include lisp implementation dependency.
|#

(defun daemonize (&key exit-parent)
  (let ((pid (fork)))
    (cond ((zerop pid)
	   ;; Child
	   (unless (eql t (setsid))
	     (exit))
	   (close-fd-streams)
	   (let* ((out (make-two-way-stream 
			(make-concatenated-stream)
			(make-broadcast-stream))))
	     (setf *standard-output* out
		   *error-output* out
		   *trace-output* out		   
		   *standard-input* out
		   *debug-io* out
		   *query-io* out
		   *terminal-io* out)))
	  (t 
	   ;; Parent
	   (when exit-parent
	     (exit))
	   (pclose 0)
	   (pclose 1)
	   (pclose 2)
	   (return-from daemonize pid))))
  nil)

(defmacro with-daemonize (&body body)
  `(progn
     (daemonize :exit-parent t)
     ,@body
     (exit)))

#|
Frome here by are not so important with daemonize. These are used for my daemon-project.
I consider separate to another file.
|#

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
