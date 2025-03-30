;;;; Smalltalk-inspired CLOS

(in-package :first-project)

;; Inspired by Smalltalk's collect: message
;; #(1 2 3) collect: [:x | x + 2]

(defgeneric collect (collection function))

(defmethod collect ((collection list) function)
  (mapcar function collection))

(collect '(1 2 3) (lambda (x) (+ 1 x)))

(defmethod collect ((collection vector) function)
  (map 'vector function collection))

(collect (vector 1 2 3) (lambda (x) (+ 1 x)))

;; 1 > 2 ifTrue: [ 'one is greater' ]

(defmacro block[] (body)
  `(lambda () ,body))

(funcall (block[] 2)) ;; -> 2

(defgeneric if-true (condition outcome))
(defgeneric if-false (condition outcome))
(defgeneric if-else (condition then else))

(defclass bool ()
  ())

(defclass true (bool)
  ())

(defmethod if-true ((condition true) (outcome function))
  (funcall outcome))

(defmethod if-false ((condition true) (outcome function))
  nil)

(defmethod if-else ((condition true) (then function) (else function))
  (funcall then))

(defclass false (bool)
  ())

(defmethod if-true ((condition false) _)
  nil)

(defmethod if-false ((condition false) (outcome function))
  (funcall outcome))

(defmethod if-else ((condition false) (then function) (else function))
  (funcall else))

(defconstant +true (make-instance 'true))
(defconstant +false (make-instance 'false))

(defgeneric gt (thing other-thing))

(defmethod gt ((thing real) (other-thing real))
  (if (> thing other-thing)
      +true
	  +false))

(if-true (gt 2 1)
		 (block[] 2))

(defgeneric maxi (one other))

(defmethod maxi ((one real) (other real))
	(if-else (gt one other)
	 (block[] one)
	 (block[] other)))

(maxi 3 4)

(defmethod == ((one number) (other number))
  (if (= one other)
	  +true
	  +false))

(defmethod reciprocal ((one number))
  (if-false (== one 0)
			(block[] (/ 1 one))))

;; if RECIPROCAL is given 0, then == returns FALSE, meaning that IF-FALSE will be sent to the FALSE object
;; if RECIPROCAL is given a non-zero number, then == returns the TRUE object and the IF-FALSE message  will be sent to the TRUE object

(reciprocal 0) ;; => NIL
(reciprocal 2) ;; => 1 / 2

;;; I realised T and NULL were classes!

(defmethod if-true ((condition t) (outcome function))
  (funcall outcome))

(defmethod if-false ((condition t) (outcome function))
  nil)

(defmethod if-else ((condition t) (then function) (else function))
  (funcall then))

(defmethod if-true ((condition null) (outcome function))
  nil)

(defmethod if-false ((condition null) (outcome function))
  (funcall outcome))

(defmethod if-else ((condition null) (then function) (else function))
  (funcall else))

(defun reciprocal-2 (num)
  (if-false (= 0 num)
			(block[] (/ 1 num))))

(reciprocal-2 0)
(reciprocal-2 3)
