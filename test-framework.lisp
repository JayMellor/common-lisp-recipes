(in-package :first-project)

(defvar *test-func-name* nil
  "Dynamic variable used to store name of current function")

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ~a: ~a~%" result *test-func-name* form)
  result)

;; using a function
(defun func-and (&rest params)
  (let ((result t))
	(dolist (param params)
	  (setf result (and result param)))
	result))

;; using a macro
(defmacro combine (&body body)
  (with-gensyms (result)
	`(let ((,result t))
	  ,@(loop for form in body
			collect `(unless ,form
					   (setf ,result nil)))
	   ,result)))

(combine (zerop 1) (= 2 3) (+ 2 3))

(defmacro check (&body forms)
  `(combine
	 ,@(loop for form in forms
		 collect `(report-result ,form ',form))))

(check (= (+ 1 2) 3)
	   (= (+ -2 -5) -7))

(defmacro deftest (name lambda-list &body body)  
  `(defun ,name ,lambda-list
	 (let ((*test-func-name*
			 (append *test-func-name* (list ',name))))
	   ,@body)))

(deftest test-+ ()  
  (check
	(= (+ 1 2) 3)
	(= (+ -2 -5) -7)
	(= (+ 3 9) 12)))

(deftest test-* ()  
  (check
	(= (* 1 1) 1)
	(= (* -1 5) -5)
	(= (* -3 -4) 12)))

(deftest test-arithmetic ()
  (combine
	(test-+)
	(test-*)))
