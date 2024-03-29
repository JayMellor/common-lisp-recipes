
;; Probably want
;; (setq *read-default-float-format* 'double-float)

(defparameter *k* 3.0 "'Dial' constant")

(defun next-step (input)
  "x_n+1 = kx_n(1 - x_n)"
  (* *k* input (- 1 input)))

(defun print-steps (x0 &key (steps 10) (k-val 0 k-specified?))
  "Prints outputs of successive applications of NEXT-STEP."
  (let ((input x0)
		(k (if k-specified? k-val *k*)))
	(format t "For x0 = ~A and k = ~A~%" x0 k)
	(format t "~,5f~%" x0)
	(let ((results (interaction-results input :steps steps :k-val k)))
	  (format t "~{~,5f~^~%~}" results))))

(defun interaction-results (input &key (steps 10)
								   (k-val 0 k-specified?)
								   (step-fn #'next-step))
  (let ((results nil))
  	(dotimes (idx steps)
	  (let* ((*k* (if k-specified? k-val *k*))
			 (output (funcall step-fn input)))
		(setf results (cons output results))
        (setf input output)))
	(reverse results)))

(defun bifurcation-diagram (&key (x0 0.3) (steps 10))
  (loop for *k* from 0 to 4.2 by 0.2
		for result = (interaction-results x0 :k-val *k* :steps steps)
		collect result))

;; (loop for result in (bifurcation-diagram :steps 100)
;; 	  collect (nthcdr 80 result))
