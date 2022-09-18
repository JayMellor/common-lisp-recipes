(asdf:defsystem #:first-project
  :serial t
  :depends-on (#:clog)
  :components ((:file "package")
	       (:file "main")
	       (:file "clog")
	       (:file "bordeaux")))
