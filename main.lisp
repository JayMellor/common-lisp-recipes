;;;; Miscellaneous functions and recipes

(in-package :first-project)

(defun friend ()
  "Returns a string"
  "You're my friendly fiend")

;; this evaluates to itself
((lambda (x) (list x (list 'quote x)))
 '(lambda (x) (list x (list 'quote x))))
;; => ((LAMBDA (X) (LIST X (LIST 'QUOTE X))) '(LAMBDA (X) (LIST X (LIST 'QUOTE X))))

(defun another (s) (+ 1 s))

(disassemble #'another)

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

;; can be done using DOTIMES

(defun ^ (a b)
  "Custom implementation of EXPT"
  (when (> b 0)
    (reduce '* (loop for i from 0 below b collect a))))

;; NOTE
(plusp 3)
;; => T
(plusp -1)
;; => NIL

(defun rec-^ (a exp)
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

(defun plot (fn min max step)
  (loop for idx from min to max by step do
    ;; LOOP REPEAT takes a number of times to compute the expression
    (let ((times (funcall fn idx)))
      (loop repeat times  do
		(format t "*")))
    (format t "~%")))

(defmacro my-cond (&body body)
  ;; (if 'first 'second)
  ;; (if 'third 'fourth (if 'first 'second))
  ;; (if 'fifth 'sixth (if 'third 'fourth (if 'first 'second)))
  ;; etc..
  "COND defined with REDUCE"
  (reduce (lambda (acc curr)
	    (let* ((condition (car curr))
			   (consequent (cdr curr))
			   (statement `(if ,condition (progn ,@consequent))))
          (if (null acc)
			  statement
			  ;; Either statement below works here
			  ;;(append statement (list acc))
			  `(,@statement ,acc))))
		  (reverse body)
		  :initial-value nil))

(defmacro my-or (&body body)
  "OR defined with MY-COND"
  `(my-cond
     ,@(mapcar #'(lambda (element)
				   (list element element))
			   body)))

(defmacro my-and (&body body)
  "AND defined with MY-COND"
  `(my-cond
     ,@(mapcar #'(lambda (element)
				   (list (null element) element))
			   body)
     ,(list t (car (last body)))))


;;; LOOPS


(defparameter my-plist '(:one 2
						 :three 44))

;; Increments each value in MY-lIST
(dolist (num my-plist)
  (when (symbolp num)
	(incf (getf my-plist num))))

my-plist

(let ((num-list '(1 2 3 4)))
  (dolist (num num-list)
	(when (oddp num)
	  (return num)))) ;; => 1

(dotimes (idx 6)
  (format t "~a elephant..~%" (1+ idx)))

(do ((x 1 (1+ x))
	 (y 9 (1- y)))
	((= 10 x) y)
  (progn
	(dotimes (idx x)
	  (format t "."))
	(dotimes (idx y)
	  (format t "+"))
	(format t "~%")))
;; Prints .s and +s, but returns 0

;; todo try hash mapping using DO

(loop for x = 1 then (1+ x)
	  and y = 9 then (1- y)
	  while (<= x 10)
	  collecting (list x y))
;; => ((1 9) (2 8) ...)


;;; MACROS

(defun is-prime (num)
  (cond
	((<= num 1)
	 nil)
	((= 2 num)
	 t)
	((zerop (mod num 2))
	 nil)
	((null (divisor-upto num 3))
	 t)))

(defun divisor-upto (num start)
  "Finds a divisor of NUM between START and (SQRT NUM)"
  (let ((limit (isqrt num)))
	(loop for idx from start to limit
		  when (zerop (mod num idx))
			return idx)))

;; Can do LOOP ... NEVER

(defun next-prime (num)
  "Finds the next prime >= NUM"
  (loop for idx from num
		when (is-prime idx)
		  return idx))

;; Solution using DO
(do ((prime (next-prime 0) (next-prime (1+ prime))))
	((<= 19 prime) nil)
  (format t "~d~%" prime))

(defmacro do-primes-leaky ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
	   ((<= ,end ,var))
	 ,@body))

;; Example usage
(do-primes-leaky (prime 0 29)
  (format t "~d" prime))

(do-primes-leaky (prime 0 (random 100))
  (format t "~d" prime))
;; => with leaky will eval (RANDOM 100) every iteration

(defmacro do-primes-let ((var start end) &body body)
  `(do* ((start-val ,start)
		(end-val ,end)
		(,var (next-prime start-val) (next-prime (1+ ,var))))
	   ((<= end-val ,var))
	 ,@body))

;; (do-primes (end-val 0 10)
;;   (print end-val))
;; => with let causes error: END-VAL is unbound

(defmacro do-primes ((var start end) &body body)
  ;; gensym to create interned symbol
  (let ((start-val-sym (gensym))
		(end-val-sym (gensym)))
	`(do* ((,start-val-sym ,start)
		   (,end-val-sym ,end)
		   (,var (next-prime ,start-val-sym) (next-prime (1+ ,var))))
		  ((<= ,end-val-sym ,var))
	   ,@body)))

(defmacro with-gensyms (var-list &body body)
  `(let ,(loop for var in var-list
			   collect `(,var (gensym)))
	 ,@body))

;; Example usage of with-gensyms
(defmacro do-primes-with-gensyms ((var start end) &body body)  
  (with-gensyms (start-val-sym end-val-sym)
	`(do* ((,start-val-sym ,start)
		   (,end-val-sym ,end)
		   (,var (next-prime ,start-val-sym) (next-prime (1+ ,var))))
		  ((<= ,end-val-sym ,var))
	   ,@body)))

;;; NUMBERS & CHARACTERS

;; Rational
1/2
3/4

;; Complex
(defvar num #c(2 3))
(imagpart num) ;; => 3
(abs num) ;; absolute value from origin

(char= #\q #\q)


;;; COLLECTIONS

(vector 1 2 3)
#(1 2 3)

(make-array 3 :initial-element 29)
;; => #(29 29 29)

;; Resizable vector
(defparameter *my-array* (make-array 5 :fill-pointer 0))
(vector-push 'first *my-array*)
(vector-push 'second *my-array*)
(vector-pop *my-array*) 
*my-array* ; Can have up to 5 elements

(defparameter *adjustable* (make-array 5 :fill-pointer 0 :adjustable t))
(vector-push-extend 'first *adjustable*)
*adjustable*

(defparameter *str-are-vec* (make-array 5
										:fill-pointer 0
										:adjustable t
										:element-type 'character))
(vector-push-extend #\a *str-are-vec*)
*str-are-vec*

(position 'first '(one two first))

;; COMPLEMENT
(let ((not-zerop (complement #'zerop)))
  (funcall not-zerop 4))
;; => T

(position 'first '(first second first third)
		  :from-end t)
;; => 2


;; C-c C-i for autocomplete
;; Can use SETF GETHASH inside MAPHASH to edit the current entry
;; Can use REMHASH inside MAPHASH to remove entries

(defparameter *descending-list* '(4 3 2 1))
(sort *descending-list* #'<)
*descending-list*
;; => (3 4)
;; SORT destroys SEQUENCE and returns SEQUENCE in sorted order

(maplist #'(lambda (el) (1+ (car el))) '(1 2 3))
;; => (2 3 4)

;; MAPCAN is a little like FLATMAP
(defparameter *mapcan-list* '(2 3 4 5))
(mapcan #'(lambda (el)
            (if (zerop (mod el 2))
				(list el)
				nil))
		*mapcan-list*)
;; => (2 4)

;; 1 2 3 + *
;; (* 1 (+ 2 3))

(defmacro pl (&rest args)
  "Reverse Polish Notation parser"
  `(let ((num-stack nil)
		(operators '(+ *)))
	(loop for el in ',args
		  do (if (null (find el operators))
				 (when (numberp el)
				   (push el num-stack))
				 (if (< (length num-stack) 2)
					 (error "invalid form - operator too early")
					 (let* ((fst (pop num-stack))
							(scd (pop num-stack))
							(result (funcall el fst scd)))                       
					   (push result num-stack)))))
	 (if (> (length num-stack) 1)
		 (error "invalid form - expected an operator")
		 (first num-stack))))

(pl 2 3 3 + *)
;; => 12


;;; CONS USAGES

;; Trees and lists

(defparameter *my-cons* '(1 . ((2 . 5) . 3)))
(defparameter *list-copy* (copy-list *my-cons*))
(defparameter *tree-copy* (copy-tree *my-cons*))
(setf (caadr *my-cons*) 9)
*list-copy*
;; => (1 (9 5) . 3)
*tree-copy*
;; => (1 (2 5) . 3)

;; Sets
(defparameter *my-set* '(1 2 5 3))
(adjoin 5 *my-set*)
;; => (1 2 5 3)
;; Only adds if not already present
;; PUSHNEW mofidies the original list

;; associative lists

(defparameter *alist* '((a . 1) (b . 2) (c . 3)))
(assoc 'b *alist*)
;; => (B . 2)
(rassoc 3 *alist*)
;; => (C . 3)
(acons 'd 5 *alist*)
;; puts (D . 5) at the front

(pairlis '(q w e r) '(4 3 2 5))
;; => ((Q . 4) (W . 3) (E . 2) (R . 5))

;; property lists

'(a 1 b 2 c 3)

;;; Auto Lisp example

(defmacro repeat (count &body body)
  (let ((times-var (gensym)))
	`(dotimes (,times-var ,count)
	   ,@body)))

;; GETVAR gets the value of the variable if it's a symbol
(defun getvar (symbol)
  (when (symbolp symbol)
	(eval symbol)))

(defparameter MLST nil)

(defun modes (a)
  "Takes a list of symbols and sets MLST to pairs of each symbol and its value"
  (setq MLST nil)
  (repeat (length a)
	(setq MLST (append MLST
					   (list (list (car a)
								   (getvar (car a))))))
	(setq a (cdr a))))


;; arg-overloaded function
;; (defunv fun-name ((() nil) ((a) (a))) ((a b) (+ a b)))
;; (fun-name 1) -> 1
;; (fun-name) -> nil
;; Doesn't support & keywords
(defmacro defunv (name &body bodies)
  (when (and (listp bodies)
			 (every #'listp bodies)
			 (every #'(lambda (body)
					   (listp (first body)))
					bodies))
    `(defun ,name (&rest args)
	   (case (length args)
		 ,@(mapcar #'(lambda (body)
					  (let ((lambda-list (first body))
							(lambda-body (rest body)))						
						(cons (length lambda-list)                              
							  `((apply #'(lambda ,lambda-list ,@lambda-body)
									   args)))))
		   bodies)))))

(defunv addl
  (() 0)
  ((a) a)
  ((a b) (+ a b))
  ((a b c) (+ a b c)))

(addl) ;; => 0
(addl 1) ;; => 1
(addl 3 4) ;; => 7
(addl 1 2 3)

(multiple-value-bind (r err) (ignore-errors (error "some error"))
  (format nil "r: ~A;err: ~A" r err))
;; "r: NIL;err: some error"

(multiple-value-bind (r err) (ignore-errors (list "some error"))
  (format nil "r: ~A;err: ~A" r err))
;; "r: (some error);err: NIL"
