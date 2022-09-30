;; Miscellaneous functions and recipes
(in-package :first-project)

(defun friend ()
  "Returns a string"
  "You're my friendly fiend")

(defun another (s) (+ 1 s))

(defun factorial (num)
  (if (<= num 1)
      1
      (reduce #'* (loop for x from 1 to num collect x))))

; Can probably be done with reduce instead
(defmacro my-let* (bindings &body body)
  "Version of LET*. Creates locally scoped variables using BINDINGS"
  (let ((rev-bindings (reverse bindings))) 
    (let ((first-binding (car rev-bindings))
	  (cdr-bindings (cdr rev-bindings))) 
      (let ((let-bindings (concatenate 'list
				       `(let ,(list first-binding)) 
				       body))) 
	(loop for binding in cdr-bindings do 
	  (setq let-bindings `(let ,(list binding) ,let-bindings)))
	let-bindings))))

(defun flip (func)
  (lambda (fst scd)
    (funcall func scd fst)))

(defun survey (&key name)
  "An example of &KEY being optional"
  (let* ((id 1)
	 (details `(:id ,id)))
    (prog1 (if name
	       (append details `(:name ,name))
	       details)
      ; Optional print
      (print (list "details" details)))))

(loop for (src trg wgt)
	in '((1 2 3) (2 1 3) (2 3 1))
      collect (list src trg)) ; => ((1 2) (2 1) (2 3))

;; nicer hash syntax
(defmacro hash (&body k-v-pairs)
  (let* ((new-hash (make-hash-table)))
    (loop for (key value) in k-v-pairs
	  do (setf (gethash key new-hash) value))
    new-hash))

;; Memo function

(defun memo (fn)
  "Memoizes FN"
  (let ((prev-calls (make-hash-table :test #'equal))) 
    (lambda (&rest args)
      (multiple-value-bind (val is-member) (gethash args prev-calls) 
	(if is-member
	    val
	    (let ((new-val (apply fn args)))
	      (progn
		(setf (gethash args prev-calls) new-val)
		new-val)))))))

(defun ^ (a b)
  (when (> b 0)
    (reduce '* (loop for i from 0 below b collect a))))

(defun ^ (a exp)
  "Exponent function using recursion"
  (if (< exp 0)
      (/ 1 (^ a (* -1 exp)))
      (if (= exp 0)
	  1
	  (* a
	     (^ a (1- exp))))))

;; FLET allows locally scoped function creation
(flet ((f (x)
	 (+ x 2)))
  (f 2)) ;; => 4

;; (f o g)(x) = f(g(x))
;; ((o f g) x) === (f (g x))
(defun compose (func1 func2)
  (flet ((composed (&rest args)
	   (funcall func1 (apply func2 args))))
    (function composed)))

;; LABELS allows sequential function definition
(labels ((add2 (x)
	   (+ x 2))
	 (mult3 (x)
	   (* x 3))
	 (add-then-mult (x)
	   (mult3 (add2 x))))
  (add-then-mult 2))

(flet ((add2 (x)
	 (+ x 2))
       (mult3 (x)
	 (* x 3)))
  (funcall (compose #'add2 #'mult3) 3))

;; Example of generic keyword arguments
(defun kw (&rest args &key &allow-other-keys)
  args)
(kw :an-arg "fred") ;; -> (:AN-ARG "fred'")
