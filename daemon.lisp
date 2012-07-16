(defpackage :daemon
  (:use :cl)
  (:export #:exit
	   #:daemonize))

(in-package :daemon)

;;I don't know why but "sudo sbcl" lacks sb-posix
#+sbcl(require :sb-posix)

#+allegro
(eval-when (:load-toplevel :execute)
  (unless (excl.osi:detach-from-terminal-supported-p)
    (error "not supported allegro cl to detach console")))

(defconstant +default-mask+ (if (boundp '+default-mask+) +default-mask+ #o022))
(defconstant +default-mode+ (if (boundp '+default-mode+) +default-mode+ #o600))

(defconstant +wnohang+ (if (boundp '+wnohang+) +wnohang+
			   #+sbcl 1
			   #+allegro 1
			   #+ccl #.(read-from-string "#$WNOHANG")))

(defvar *o-rdonly* (or 
		    #+sbcl sb-posix:o-rdonly
		    #+allegro excl::*o-rdonly*
		    #+ccl #.(read-from-string "#$O_RDONLY")
		    ))

(defvar *o-wronly* (or
		    #+sbcl sb-posix:o-wronly
		    #+allegro excl::*o-wronly*
		    #+ccl #.(read-from-string "#$O_WRONLY")
		    ))

(defvar *o-append* (or 
		    #+sbcl sb-posix:o-append
		    #+allegro excl::*o-append*
		    #+ccl #.(read-from-string "#$O_APPEND")
		    ))

(defun chdir (dir)
  #+sbcl (sb-posix:chdir dir)
  #+allegro (excl:chdir dir)
  #+ccl (ccl::%chdir dir))

(defun dup2  (old new)
  #+sbcl (sb-posix:dup2 old new)
  #+allegro (excl.osi::syscall-dup2 old new)
  #+ccl (#.(read-from-string "#_dup2") old new))

(defun fork ()
  (or
   #+sbcl (sb-posix:fork)
   #+allegro (excl.osi:fork)
   #+ccl (#.(read-from-string "#_fork"))
   (error "should support fork")))

#-allegro
(defun popen (fspec flags)
  #+sbcl (sb-posix:open fspec flags)
  #+ccl (ccl:with-cstrs ((cstr fspec)) (#.(read-from-string "#_open") cstr flags)))

#+allegro 
(ff:def-foreign-call (popen "open") ((path (* :char)) (flags :int)) :strings-convert t)

#-allegro
(defun pclose (fd)
  #+sbcl (sb-posix:close fd)
  #+ccl (#.(read-from-string "#_close") fd))

#+allegro
(ff:def-foreign-call (pclose "close") (d)) 

(defun exit ()
  #+sbcl 
  #.(if (and (find-package :sb-posix) 
	     (find-symbol (symbol-name :exit) :sb-posix))
	(read-from-string "(sb-posix:exit 0)")
	(read-from-string "(sb-unix:unix-exit)"))
  #+allegro (excl:exit 0 :quiet t)
  #+ccl (#.(read-from-string "#_exit") 0))

(defun waitpid (pid option)
  #+sbcl(zerop (sb-posix:waitpid pid option))
  #+allegro (not (excl.osi:waitpid pid :wnohang (not (zerop option))))
  #+ccl (zerop (#.(read-from-string "#_waitpid") pid 0 option)))

(defun setsid ()
  #+sbcl (not (minusp (sb-posix:setsid))) 
  #+allegro (excl.osi:setsid)
  #+ccl (not (minusp (#.(read-from-string "#_setsid")))))

(defun umask (mask)
  #+sbcl (sb-posix:umask mask)
  #+allegro (excl.osi:umask mask)
  #+ccl (#.(read-from-string "#_umask") mask))

(defun setuid (uid)
  #+sbcl(sb-posix:setuid uid)
  #+allegro (excl.osi:setuid uid)
  #+ccl (#.(read-from-string "#_setuid") uid))

(defun getpid ()
  #+sbcl (sb-posix:getpid)
  #+allegro (excl.osi:getpid)
  #+ccl (ccl::getpid))

(defun getppid ()
  #+sbcl (sb-posix:getppid)
  #+allegro (excl.osi:getppid)
  #+ccl (#.(read-from-string "#_getppid")))

(defun getuid ()
  #+sbcl (sb-posix:getuid)
  #+allegro (excl.osi:getuid)
  #+ccl (#.(read-from-string "#_getuid")))

(defun getgrnam (name)
  #+sbcl (sb-posix:getgrnam name)
  #+allegro (excl.osi:getgrnam name)
  #+ccl (ccl::with-cstrs ((cstr name)) (#.(read-from-string "#_getgrnam") cstr)))

(defun setgid (gid)
  #+sbcl (sb-posix:setgid gid)
  #+allegro (excl.osi:setgid gid)
  #+ccl (#.(read-from-string "#_setgid") gid))

(defun getgrgid (gid)
  #+sbcl (sb-posix:getgrgid gid)
  #+allegro (excl.osi:getgrgid gid)
  #+ccl (#.(read-from-string "#_getgrgid") gid))

(defun group-gid (grp)
  #+sbcl (sb-posix:group-gid grp)
  #+allegro (excl.osi:grent-gid grp)
  #+ccl (unless (ccl:%null-ptr-p grp) (ccl:pref grp :group.gr_gid)))

(defun getpwnam (name)
  #+sbcl (sb-posix:getpwnam name)
  #+allegro (excl.osi:getpwnam name)
  #+ccl (ccl::with-cstrs ((cstr name)) (#.(read-from-string "#_getpwnam") cstr)))

(defun getpwuid (id)
  #+sbcl (sb-posix:getpwuid id)
  #+allegro (excl.osi:getpwuid uid)
  #+ccl (#.(read-from-string "#_getpwuid") id))

(defun uid-username (uid)
  #+sbcl (sb-posix:passwd-name (getpwuid uid))
  #+allegro (excl.osi:pwent-name (getpwuid uid))
  #+ccl (ccl:%get-cstring (ccl:pref (getpwuid uid) :passwd.pw_name)))

(defun passwd-uid (pswd)
  #+sbcl(sb-posix:passwd-uid pswd)
  #+allegro (excl.osi:pwent-uid pswd)
  #+ccl (unless (ccl:%null-ptr-p pswd) (ccl:pref pswd :passwd.pw_uid)))

(defun passwd-dir (pswd)
  #+sbcl (sb-posix:passwd-dir pswd)
  #+allegro (excl.osi:pwent-dir pswd)
  #+ccl (ccl:%get-cstring(ccl:pref pswd :passwd.pw_dir)))

(defun detouch-terminal (&key input output error)
  (declare (ignorable input output error))
  #-allegro
  (progn 
    (chdir "/")
    (unless (eql t (setsid))
      (exit))
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
    (dup2 (popen "/dev/null" *o-rdonly*) 0)
    (dup2 (popen "/dev/null" (logior *o-wronly* *o-append*)) 1)
    (dup2 (popen "/dev/null" *o-rdonly*) 2))
  #+allegro 
  (excl.osi:detach-from-terminal :output-stream nil :error-output-stream nil))

(defun daemonize (&key input output error (umask +default-mask+) pidfile
		  exit-parent (exit-hook t) (disable-debugger t)
		  user group
		  sigabrt sighup sigint sigterm)
  (declare (ignorable exit-hook disable-debugger sigabrt sighup sigint sigterm))
  (when pidfile
    (ignore-errors (delete-file pidfile)))
  (let ((uid (typecase user
	       (string
		(or (passwd-uid (getpwnam user))
		    (error "Unknown username: ~S" user)))
	       (unsigned-byte
		(or (passwd-uid user)
		    (error "Unknown userid: ~S" user)))))
	(gid (typecase group
	       (string 
		(group-gid 
		 (or (getgrnam group)
		     (error "Unknown groupname: ~S" group))))
	       (unsigned-byte
		(if (getgrgid group)
		    group
		    (error "Unknown groupid: ~S" group)))))
	(pid (fork)))
    (cond ((zerop pid)
	   ;; Child
	   (umask umask)
	   (detouch-terminal :input input :output output :error error)
	   (when gid
	     (setgid gid))
	   (when uid 
	     (setuid uid))
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
  (when pidfile
    (with-open-file (f pidfile :direction :output
		       :if-exists :supersede)
      (format f "~A~%" (getpid))))
  nil)
