(in-package :daemon)

#+allegro
(eval-when (:load-toplevel :execute)
  (unless (excl.osi:detach-from-terminal-supported-p)
    (error "not supported allegro cl to detach console")))

#+ecl
(ffi:clines "#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <grp.h>
#include <pwd.h>
#include <sys/stat.h>
")


(defconstant +default-mask+ (if (boundp '+default-mask+) +default-mask+ #o022))
(defconstant +default-mode+ (if (boundp '+default-mode+) +default-mode+ #o600))

(defvar *wnohang* (or
                   #+sbcl 1
                   #+allegro 1
                   #+ccl #.(read-from-string "#$WNOHANG")
                   #+ecl (ffi:c-inline () () :int "WNOHANG" :one-liner t)
                   ))

(defvar *o-rdonly* (or
                    #+sbcl sb-posix:o-rdonly
                    #+allegro excl::*o-rdonly*
                    #+ccl #.(read-from-string "#$O_RDONLY")
                    #+ecl (ffi:c-inline () () :int "O_RDONLY" :one-liner t)
                    ))

(defvar *o-wronly* (or
                    #+sbcl sb-posix:o-wronly
                    #+allegro excl::*o-wronly*
                    #+ccl #.(read-from-string "#$O_WRONLY")
                    #+ecl (ffi:c-inline () () :int "O_WRONLY" :one-liner t)
                    ))

(defvar *o-append* (or
                    #+sbcl sb-posix:o-append
                    #+allegro excl::*o-append*
                    #+ccl #.(read-from-string "#$O_APPEND")
                    #+ecl (ffi:c-inline () () :int "O_APPEND" :one-liner t)
                    ))

(defun chdir (dir)
  #+sbcl (sb-posix:chdir dir)
  #+allegro (excl:chdir dir)
  #+ccl (ccl::%chdir dir)
  #+ecl (ffi:c-inline (dir) (:cstring) :int
                      "chdir(#0)" :one-liner t))

(defun dup2  (old new)
  #+sbcl (sb-posix:dup2 old new)
  #+allegro (excl.osi::syscall-dup2 old new)
  #+ccl (#.(read-from-string "#_dup2") old new)
  #+ecl (ffi:c-inline (old new) (:int :int) :int
                      "dup2(#0,#1)" :one-liner t))

(defun fork ()
  (or
   #+sbcl (sb-posix:fork)
   #+allegro (excl.osi:fork)
   #+ccl (#.(read-from-string "#_fork"))
   #+ecl (ffi:c-inline () () :int
                       "(int) fork()" :one-liner t)
   (error "should support fork")))

#-allegro
(defun popen (fspec flags)
  #+sbcl (sb-posix:open fspec flags)
  #+ccl (ccl:with-cstrs ((cstr fspec)) (#.(read-from-string "#_open") cstr flags))
  #+ecl (ffi:c-inline (fspec flags) (:cstring :int) :int
                      "open(#0,#1)" :one-liner t))

#+allegro
(ff:def-foreign-call (popen "open") ((path (* :char)) (flags :int)) :strings-convert t)

#-allegro
(defun pclose (fd)
  #+sbcl (sb-posix:close fd)
  #+ccl (#.(read-from-string "#_close") fd)
  #+ecl (ffi:c-inline (fd) (:int) :int
                      "close(#0)" :one-liner t))

#+allegro
(ff:def-foreign-call (pclose "close") (d))

(defun exit ()
  #+sbcl
  #.(if (and (find-package :sb-posix)
             (find-symbol (symbol-name :exit) :sb-posix))
        (read-from-string "(sb-posix:exit 0)")
        (read-from-string "(sb-unix:unix-exit)"))
  #+allegro (excl:exit 0 :quiet t)
  #+ccl (#.(read-from-string "#_exit") 0)
  #+ecl (ext:exit))

(defun waitpid (pid option)
  #+sbcl(zerop (sb-posix:waitpid pid option))
  #+allegro (not (excl.osi:waitpid pid :wnohang (not (zerop option))))
  #+ccl (zerop (#.(read-from-string "#_waitpid") pid 0 option))
  #+ecl (ffi:c-inline (pid option) (:int :int) :int
                      "waitpid(#0,NULL,#1)" :one-liner t))

(defun setsid ()
  #+sbcl (not (minusp (sb-posix:setsid)))
  #+allegro (excl.osi:setsid)
  #+ccl (not (minusp (#.(read-from-string "#_setsid"))))
  #+ecl (not (minusp (ffi:c-inline () () :int
                                   "setsid()" :one-liner t))))

(defun umask (mask)
  #+sbcl (sb-posix:umask mask)
  #+allegro (excl.osi:umask mask)
  #+ccl (#.(read-from-string "#_umask") mask)
  #+ecl (ffi:c-inline (mask) (:int) :int
                      "umask(#0)" :one-liner t))

(defun setuid (uid)
  #+sbcl(sb-posix:setuid uid)
  #+allegro (excl.osi:setuid uid)
  #+ccl (#.(read-from-string "#_setuid") uid)
  #+ecl (ffi:c-inline (uid) (:int) :int
                      "setuid(#0)" :one-liner t))

(defun getpid ()
  #+sbcl (sb-posix:getpid)
  #+allegro (excl.osi:getpid)
  #+ccl (ccl::getpid)
  #+ecl (ffi:c-inline () () :int
                      "getpid()" :one-liner t))

(defun getppid ()
  #+sbcl (sb-posix:getppid)
  #+allegro (excl.osi:getppid)
  #+ccl (#.(read-from-string "#_getppid"))
  #+ecl (ffi:c-inline () () :int
                      "getppid()" :one-liner t))

(defun getuid ()
  #+sbcl (sb-posix:getuid)
  #+allegro (excl.osi:getuid)
  #+ccl (#.(read-from-string "#_getuid"))
  #+ecl (ffi:c-inline () () :int
                      "getuid()" :one-liner t))

(defun getgrnam (name)
  #+sbcl (sb-posix:getgrnam name)
  #+allegro (excl.osi:getgrnam name)
  #+ccl (ccl::with-cstrs ((cstr name)) (#.(read-from-string "#_getgrnam") cstr))
  #+ecl (let ((ret (ffi:c-inline (name) (:cstring) :pointer-void
                                 "getgrnam(#0)" :one-liner t)))
          (unless (zerop (si:foreign-data-address ret))
            ret)))

(defun setgid (gid)
  #+sbcl (sb-posix:setgid gid)
  #+allegro (excl.osi:setgid gid)
  #+ccl (#.(read-from-string "#_setgid") gid)
  #+ecl (ffi:c-inline (gid) (:int) :int
                      "setgid(#0)" :one-liner t))

(defun getgrgid (gid)
  #+sbcl (sb-posix:getgrgid gid)
  #+allegro (excl.osi:getgrgid gid)
  #+ccl (#.(read-from-string "#_getgrgid") gid)
  #+ecl (ffi:c-inline (gid) (:int) :pointer-void
                      "getgrgid(#0)" :one-liner t))

(defun group-gid (grp)
  #+sbcl (sb-posix:group-gid grp)
  #+allegro (excl.osi:grent-gid grp)
  #+ccl (unless (ccl:%null-ptr-p grp) (ccl:pref grp :group.gr_gid))
  #+ecl (ffi:c-inline (grp) (:pointer-void) :int
                      "((struct group*)(#0))->gr_gid" :one-liner t))

(defun getpwnam (name)
  #+sbcl (sb-posix:getpwnam name)
  #+allegro (excl.osi:getpwnam name)
  #+ccl (ccl::with-cstrs ((cstr name)) (#.(read-from-string "#_getpwnam") cstr))
  #+ecl (let ((ret (ffi:c-inline (name) (:cstring) :pointer-void
                                 "getpwnam(#0)" :one-liner t)))
          (unless (zerop (si:foreign-data-address ret))
            ret)))

(defun getpwuid (id)
  #+sbcl (sb-posix:getpwuid id)
  #+allegro (excl.osi:getpwuid id)
  #+ccl (#.(read-from-string "#_getpwuid") id)
  #+ecl (let ((ret (ffi:c-inline (id) (:int) :pointer-void
                                 "getpwuid(#0)" :one-liner t)))
          (unless (zerop (si:foreign-data-address ret))
            ret)))

(defun passwd-username (pswd)
  #+sbcl (sb-posix:passwd-name pswd)
  #+allegro (excl.osi:pwent-name pswd)
  #+ccl (ccl:%get-cstring (ccl:pref pswd :passwd.pw_name))
  #+ecl (when pswd
          (ffi:c-inline (pswd) (:pointer-void) :cstring
                        "((struct passwd*)(#0))->pw_name" :one-liner t)))

(defun passwd-uid (pswd)
  #+sbcl(sb-posix:passwd-uid pswd)
  #+allegro (excl.osi:pwent-uid pswd)
  #+ccl (unless (ccl:%null-ptr-p pswd) (ccl:pref pswd :passwd.pw_uid))
  #+ecl (when pswd
          (ffi:c-inline (pswd) (:pointer-void) :int
                        "((struct passwd*)(#0))->pw_uid" :one-liner t)))

(defun passwd-dir (pswd)
  #+sbcl (sb-posix:passwd-dir pswd)
  #+allegro (excl.osi:pwent-dir pswd)
  #+ccl (ccl:%get-cstring(ccl:pref pswd :passwd.pw_dir))
  #+ecl (when pswd
          (ffi:c-inline (pswd) (:pointer-void) :cstring
                        "((struct passwd*)(#0))->pw_dir" :one-liner t)))

(defun detouch-terminal (&key input output error set-stream)
  (declare (ignorable input output error))
  #-allegro
  (progn
    (chdir "/")
    (unless (eql t (setsid))
      (exit))
    (when set-stream
      #+sbcl
      (setf sb-sys:*stdin* (make-concatenated-stream)
            sb-sys:*stdout* (make-broadcast-stream)
            sb-sys:*stderr* (make-broadcast-stream))
      #+ccl
      (setf ccl::*stdin* (make-concatenated-stream)
            ccl::*stdout* (make-broadcast-stream)
            ccl::*terminal-input* ccl::*stdin*
            ccl::*terminal-output* ccl::*stdout*
            ccl::*terminal-io* (make-two-way-stream
                                ccl::*terminal-input* ccl::*terminal-output*)))
    #+sbcl
    (when (sb-sys:fd-stream-p sb-sys:*tty*)
      (close sb-sys:*tty* :abort t)
      (setf sb-sys:*tty* (make-two-way-stream sb-sys:*stdin* sb-sys:*stdout*)))
    (dup2 (popen "/dev/null" *o-rdonly*) 0)
    (dup2 (popen "/dev/null" (logior *o-wronly* *o-append*)) 1)
    (dup2 (popen "/dev/null" (logior *o-wronly* *o-append*)) 2))
  #+allegro
  (excl.osi:detach-from-terminal :output-stream nil :error-output-stream nil))

(defun daemonize (&key input output error (umask +default-mask+) pidfile
                    exit-parent (exit-hook t) (disable-debugger t)
                    user group (set-stream t)
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
           (detouch-terminal :input input :output output :error error :set-stream set-stream)
           (when gid
             (setgid gid))
           (when uid
             (setuid uid))
           (when set-stream
             (let ((out (make-two-way-stream
                         (make-concatenated-stream)
                         (make-broadcast-stream))))
               (setf *standard-output* out
                     *error-output* out
                     *trace-output* out
                     *standard-input* out
                     *debug-io* out
                     *query-io* out
                     *terminal-io* out))))
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
