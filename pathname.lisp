;;;; Portable pathname library

(in-package :first-project)

;;; Feature flags

#+common-lisp 1
;; => 1

#+something-else 1
;; No value

(defun impl ()
  #+sbcl :sbcl
  #+allegro :allegro
  #-(or sbcl allegro) (error "Not implemented"))

(impl)
;; => :SBCL

;;; Listing directories

(directory (make-pathname :name :wild
						  :type :wild
						  :defaults "~/Development/Common Lisp/"))
;; => Lists all files in ~/Development/Common Lisp/

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun pathname-in-directory-form-p (pathname)
  (and (not (component-present-p  (pathname-name pathname)))
	   (not (component-present-p (pathname-type pathname)))))

(pathname-in-directory-form-p #p"~/Development/")
(pathname-in-directory-form-p #p"~/Development/Something")

(defun pathname->directory (pathname)
  (when (wild-pathname-p pathname)
	(error "Cannot convert wildcard path"))
  (if (pathname-in-directory-form-p pathname)
	  pathname
	  (make-pathname :directory (construct-directory pathname)
					 :name nil
					 :type nil
					 :defaults pathname)))

(defun construct-directory (pathname)
  "Converts PATHNAME into a list that can be consumed by MAKE-PATHNAME as a directory"
  (append (or (pathname-directory pathname)
			  (list :relative))
		  (list (file-namestring pathname))))

(construct-directory #p"~/Development")
;; => (:ABSOLUTE :HOME "Development")

(construct-directory #p"SomeFolder")
;; => (:RELATIVE "SomeFolder")

(make-pathname :name nil
			   :directory (construct-directory "Development")
			   :type nil
			   :defaults "Development")
(pathname-directory "~/Development")
(file-namestring "~/Development")

(defun directory->wildcard (directory-name)
  (make-pathname :name :wild
				 :type #-clisp :wild #+clisp nil
				 :defaults (pathname->directory directory-name)))

(directory->wildcard "~/Development")
;; => #P"Development/*.*"

(directory (directory->wildcard "~/Development"))
;; Lists all files in `~/Development/'

;; Doesn't work on some implementations of common lisp
(defun list-directory (directory-name)
  (when (wild-pathname-p directory-name)
	(error "Can only list concrete directory names"))
  (directory (directory->wildcard directory-name)))

(list-directory "~/Development")

;; CONSTANTLY creates a function that always returns VALUE
(funcall (constantly t) nil)
;; => T

(defun walk-directory-if(function directory-name &key directories (test (constantly t)))
  (labels
	  ((walk (name)
		 (cond
		   ((pathname-in-directory-form-p name)
			(when (and directories (funcall test name))
			  (funcall function name))
			(dolist (file (list-directory name))
			  (walk file)))
		   ((funcall test name)
			(funcall function name)))))
	(walk (pathname->directory directory-name))))

;;(walk-directory-if (lambda (name) (format t "directory: ~A~%" name)) "~/Development")
;; prints out for each file and directory in path - recursively!

;; (map-directory (lambda (name) (format t "directory: ~A~%" name)) "/Users/jaymellor/Development/")
;; => prints out files - a bit bizarre though

;; todo - write a macro `do-directory' that provides a context for each file within the provided directory
