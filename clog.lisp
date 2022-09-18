;; clog functions
(in-package :first-project)

(defun div-body (body)
  (set-on-click (create-div body :content "hello world"
				 :class "fred")
		(lambda (obj)
		  (setf (color obj) (progn
				      (terpri)
				      (princ "Setting colour to: ")
				      (print (rgb (random 255)
						  (random 255)
						  (random 255))))))))

(defun hello-world ()
  (initialize 'div-body)
  (open-browser))
