(in-package :daemon)

(defun write-config (path name base &key (if-exists :rename-and-delete))
  (let ((html-template:*string-modifier* #'identity))
    (with-open-file (out (merge-pathnames (format nil "~A" name) path) 
			 :direction :output
			 :if-exists if-exists)
      (html-template:fill-and-print-template 
       (merge-pathnames "project/template.sh" base)
       (list :opts 
	     (format nil "~{~{~A=~S~}~%~}"
		     `((:projectname ,name)
		       (:lisp ,(or (progn #+sbcl (first sb-ext:*posix-argv*)
					  #+allegro (excl::get-argv 0)
					  #+ccl (first ccl:*command-line-argument-list*)) ""))
		       (:preopt ,(or (progn #+sbcl "--load"
					    #+allegro "-L"
					    #+ccl "--load") ""))
		       (:postopt ,(or (progn #+sbcl "--no-sysinit --no-userinit"
					     #+allegro "-qq -- "
					     #+ccl "--no-init --") ""))
		       (:projectpath ,(namestring (make-pathname :name nil
								 :type nil
								 :version nil
								 :defaults path)))
		       (:loadfile "loadup.lisp"))))
       :stream out))))

(defun write-loadup (path base &key (if-exists :rename-and-delete))
  (let ((html-template:*string-modifier* #'identity))
    (with-open-file (out (merge-pathnames "loadup.lisp" path) 
			 :direction :output
			 :if-exists if-exists)
      (html-template:fill-and-print-template 
       (merge-pathnames "project/loadup.lisp" base)
       `(:qlpath ,(format nil "~S" ql:*quicklisp-home*))
       :stream out))))

(defvar *project-path* nil)

(defun new-project (path &key project-name)
  (assert (pathnamep path))
  (setq *project-path* path)
  (ensure-directories-exist path)
  (ensure-directories-exist (merge-pathnames "procs/" path))
  (ensure-directories-exist (merge-pathnames "library/daemon/" path))
  (setf project-name (or project-name (pathname-name path)))
  (let ((base (load-time-value (or #.*compile-file-pathname* *load-pathname*))))
    (flet ((copy (a)
	     (ql-util:copy-file 
	      (merge-pathnames a base)
	      (merge-pathnames a (merge-pathnames "library/daemon/" path))))
	   (chmod (name)
	     #+sbcl(sb-posix:chmod (merge-pathnames name path) #o755)
	     #+allegro(excl.osi:chmod (merge-pathnames name path) #o755)
	     #+ccl (ccl::with-cstrs ((cstr (namestring (merge-pathnames name path)))) (#_chmod cstr #o755))
	     ))
      (mapc #'copy '("daemon.lisp"))
      (write-loadup path base)
      (write-config path project-name base)
      (chmod project-name))
    t))
