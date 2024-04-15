(in-package :first-project)

;; `DEFGENERIC' defines generic functions - functions that can be called on any object
(defgeneric draw (shape)
  (:documentation "Draw the given shape on screen"))

;; Methods belong to generic functions instead of classes

(defclass circle ()
  ())

(defmethod draw ((shape circle))
  (format t "o"))

;;; Bank

(defgeneric withdraw (account amount)
  (:documentation "Withdraw AMOUNT from ACCOUNT. Signal an error if balance of ACCOUNT is less than AMOUNT"))

(defvar *last-account-number* 0)

;; Implicitly inherits from STANDARD-OBJECT
(defclass bank-account ()
  ((account-number
	:initform (incf *last-account-number*)
	:documentation "Reference for account; unique to each bank")
   (customer-name
	:initarg :customer-name
	:initform (error "Must supply a customer name"))   
   (balance
	:initarg :balance
	:initform 0
	:reader balance ; Equivalent to generic function BALANCE below
	;; :accessor balance would add reader and writer methods
	)
   (on-hold)))

;; INITIALIZE-INSTANCE :after method is equivalent to a constructor or __init__ in other languages
;; Adding OPENING-BONUS-PERCENTAGE as a key makes it an eligible key parameter for MAKE-INSTANCE
(defmethod initialize-instance :after ((account bank-account) &key opening-bonus-percentage)
  (let ((balance (slot-value account 'balance)))
	(setf (slot-value account 'on-hold)
		  (zerop balance)))
  (when opening-bonus-percentage
	(incf (slot-value account 'balance)
		  (* (slot-value account 'balance)
			 (/ opening-bonus-percentage 100)))))

;; DEFGENERIC not needed for all DEFMETHODS, but good practice for documentation purposes. BANK-ACCOUNT is the 'specializer' of ACCOUNT
(defmethod balance ((account bank-account))
  1)

;; ACCOUNT being specialized by T doesn't need to be specified - it's otherwise implicit
(defmethod withdraw ((account bank-account) (amount t))
  (when (< (balance account) amount)
	(error "overdrawn"))
  2)

(defclass current-account (bank-account)
  ())

(defclass savings-account (bank-account)
  ())

(defgeneric overdraft-account (account)
  (:documentation "Returnes the overdraft account associated with ACCOUNT"))

(defmethod withdraw ((account current-account) amount)
  (let ((overdraft (- amount (balance account))))
	(when (plusp overdraft)
	  (withdraw (overdraft-account account) overdraft)
	  (incf (balance account) overdraft)))
  (call-next-method) ; needed if you want to call the next method in the inheritance chain
  )

(defparameter *account-of-bank-president* (make-instance 'bank-account
														 :customer-name "Jeremy"
														 :balance 2000))

;; Overrides for the account matching that of the president
(defmethod withdraw ((account (eql *account-of-bank-president*)) amount)
  (call-next-method))

;; :before methods are called before standard methods, most specific first
(defmethod withdraw :before ((account current-account) amount)
  )

;; :around methods work like a context, calling CALL-NEXT-METHOD in its middle

;; (defparameter *new-account* (make-instance 'bank-account))
;; (setf (slot-value *new-account* 'customer-name) "Matthew")
;; (slot-value *new-account* 'customer-name)
;; => Matthew

;; Can supply slots when initialising an instance

(defparameter *account* (make-instance 'bank-account
									   :customer-name "Fred"))
(slot-value *account* 'account-number)
;; => 1 (last account number)
(slot-value *account* 'on-hold)
;; => T

(defparameter *bonus-acc* (make-instance 'bank-account
										 :customer-name "Oswald"
										 :balance 200
										 :opening-bonus-percentage 5))

(slot-value *bonus-acc* 'balance)
;; => 210

(defgeneric balance (account))

(defmethod balance ((account bank-account))
  (slot-value account 'balance))

(balance *bonus-acc*)

(defgeneric (setf customer-name) (name account))

(defmethod (setf customer-name) (name (account bank-account))
  (setf (slot-value account 'customer-name) name))

(defgeneric customer-name (account))

(defmethod customer-name ((account bank-account))
  (slot-value account 'customer-name))

(setf (customer-name *bonus-acc*) "Dracula")
(customer-name *bonus-acc*)
;; => Dracula

;; WITH-SLOTS exists
;; WITH-ACCESSORS exists

(defclass money-market-account (current-account savings-account)
  ()
  (:documentation "Precedence of superclasses is CURRENT-ACCOUNT then SAVINGS-ACCOUNT"))
