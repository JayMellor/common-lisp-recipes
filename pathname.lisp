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

(defun directory-pathname-p (pathname)
  (and (not (component-present-p  (pathname-name pathname)))
	   (not (component-present-p (pathname-type pathname)))))

(directory-pathname-p #p"~/Development/")
(directory-pathname-p #p"~/Development/Something")

(defun pathname-as-directory (pathname)
  (when (wild-pathname-p pathname)
	(error "Cannot convert wildcard path"))
  (if (directory-pathname-p pathname)
	  pathname
	  (make-pathname :directory (construct-directory pathname)
					 :name nil
					 :type nil
					 :defaults pathname)))

(defun construct-directory (pathname)
  (append (or (pathname-directory pathname) (list :relative))
		  (list (file-namestring pathname))))

(let ((d (pathname-as-directory "~/Development")))
  (pathname-directory d))

(make-pathname :name nil :directory (construct-directory "Development") :type nil :defaults "Development")
(pathname-directory "~/Development")
(file-namestring "~/Development")
