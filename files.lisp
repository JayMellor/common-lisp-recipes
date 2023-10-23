;;; FILES

(in-package :first-project)

(let ((in (open #p"./.gitignore"
				:if-does-not-exist nil)))
  (when in
	(loop for line = (read-line in nil)	  
		  while line do (format t "~a~%" line))
	(close in)))

(let* ((in (open #p"./.gitignore"))
	   (seq (make-array 10))
	   ;; READ-SEQUENCE reads from the file until the sequence is full
	   (upto (read-sequence seq in)))
  (format t "Up to ~a~%~a~%" upto seq))

;; READ reads sexps from a stream

;; OPEN can be used to write to files too
(let ((out (open "./file"
				 :direction :output
				 :if-exists :supersede)))
  (write-string "this is a file" out)
  (close out))

;; WITH-OPEN-FILE guarantees closure using UNWIND-PROTECT
(with-open-file (in "./.gitignore")
  (format t "~a~%" (read-line in)))

;;; PATHNAMES

(let* ((file-path (pathname "./file"))  
	  (file-path-name (pathname-name file-path))
	  (file-path-type (pathname-type file-path))
	  (file-path-dir (pathname-directory file-path)))
  (list file-path-name file-path-type file-path-dir))
;; => ("file" NIL (:RELATIVE "."))

;; PATHNAME-HOST or PATHNAME-DEVICE might return the drive on Windows

(namestring #p"/first/second/third.txt")
(directory-namestring #p"/first/second/third.txt")
;; => "/first/second/"
(file-namestring #p"/first/second/third.txt")
;; => "third.txt"

(make-pathname
 :directory '(:absolute "first" "second")
 :name "third"
 :type "ext")
;; => #P"/first/second/third.ext"

(make-pathname :directory '(:relative "backups")
			   :defaults #p"/first/second.txt")
;; => #P"backups/second.txt"

(merge-pathnames #p"dir/file.html")
;; => #P"/Users/jaymellor/Development/Common Lisp/first-project/dir/file.html"

(merge-pathnames #p"dir/file.html" #p"/www/html/")
;; => #P"/www/html/dir/file.html"

(merge-pathnames (enough-namestring #p"/www/dir/file.html" #p"/www/")
 ;; => "dir/file.html"
 #p"/www-backups/")
;; => #P"/www-backups/dir/file.html"

;; C-c C-d C-d describe

(probe-file #p"main.lisp")
;; => #P"/Users/jaymellor/Development/Common Lisp/first-project/main.lisp"

(probe-file #p"nonexistant.file")
;; => NIL

(probe-file #p"~/Development/")
;; => #P"/Users/jaymellor/Development/"
;; Implementation specific

(directory "~/")
;; => (#P"/Users/jaymellor/")
;; Supports regex, so (directory "~/*") will return all files and directories in ~/
