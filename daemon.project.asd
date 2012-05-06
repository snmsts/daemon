(defsystem :daemon.project
  :serial t
  :components ((:file "package") 
	       (:file "project"))
  :depends-on (:daemon :html-template))
