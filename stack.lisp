;;;; Stack-based language interpreter

;; 1 2 | dup => 1 2 2 |
(defun dup (stack)
  (cons (first stack) stack))

(defun swap (stack)
  `(,(second stack) ,(first stack) ,@(cddr stack)))

(defun stack-quote (stack)
  (cons (list (first stack)) (cdr stack)))

(defun add (stack)
  (cons (+ (first stack) (second stack)) (cddr stack)))

(defun mult (stack)
  (cons (* (first stack) (second stack)) (cddr stack)))

;; 1 2 | [dup * swap dup * + sqrt] apply
;; 1 2 [dup * swap dup * + sqrt] | apply
;; 1 2 | dup * swap dup * + sqrt
(defun stack-apply (stack operations)
  (values (cdr stack) `(,@(first stack) ,@operations)))

;; 1 2 [dup] [*] | compose apply
;; 1 2 [dup *] | apply
;; 1 2 | dup *
(defun compose (stack)
  (cons (append (second stack) (first stack)) (cddr stack)))

;; 1 2 dup [*] | cons
;; 1 2 [dup *] |
;; cons : v . v . [p] -> v . [v p]
(defun stack-cons (stack)
  (cons (cons (second stack) (first stack)) (cddr stack)))

(defparameter +stack-operations+
  '((dup . dup)
	(swap . swap)
	(quote . stack-quote)
	(+ . add)
	(* . mult)
	(compose . compose)
	(cons . stack-cons)))

(defparameter +stack-opn-operations+
  '((apply . stack-apply)))

;; 1 1 * 2 2 * + sqrt

(defun next-step (stack operations)
  (let ((operation (car operations))
		(remaining (cdr operations)))
	(cond
	  ((numberp operation)
	   (values (cons operation stack) remaining))
	  ((consp operation)
	   (values (cons operation stack) remaining))
	  ((symbolp operation)       
	   (let ((stack-match (assoc operation +stack-operations+))
			 (stack-opr-match (assoc operation +stack-opn-operations+)))
		 (cond ((consp stack-match)
				(values (funcall (cdr stack-match) stack) remaining))
			   ((consp stack-opr-match)
				(funcall (cdr stack-opr-match) stack remaining))))))))

(defun calculate (stack operations &optional debug)
  (loop while (not (null operations))
		do (multiple-value-bind
				 (next-stack next-operations) (next-step stack operations)
			 (when debug
			   (format t "~a | ~a~%" (reverse next-stack) next-operations))
			 (setf stack next-stack)
			 (setf operations next-operations)))
  (values stack operations))
