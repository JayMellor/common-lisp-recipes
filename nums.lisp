(defparameter *k* 3 "'Dial' constant")

(defun next-step (input)
  "x_n+1 = kx_n(1 - x_n)"
  (* *k* input (- 1 input)))

(defun print-steps (x0 &key (steps 10) (k-val 0 k-specified?))
  "Prints outputs of successive applications of NEXT-STEP."
  (let ((input x0)
		(k (if k-specified? k-val *k*)))
	(format t "For x0 = ~A and k = ~A~%" x0 k)
	(format t "~,5f~%" x0)
	(let ((results (interation-results input :steps steps :k-val k)))
	  (format t "~{~,5f~^~%~}" results))))

(defun interation-results (input &key (steps 10)
								   (k-val 0 k-specified?)
								   (step-fn #'next-step))
  (let ((results nil))
  	(dotimes (idx steps)
	  (let* ((*k* (if k-specified? k-val *k*))
			 (output (funcall step-fn input)))
		(setf results (cons output results))
        (setf input output)))
	(reverse results)))
