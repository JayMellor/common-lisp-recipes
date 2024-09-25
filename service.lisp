(in-package :first-project)

(defclass bank-account-repository ()
  ((bank-accounts
	:initform nil
	:reader bank-accounts
	:writer (setf bank-accounts))
   (last-account-number
	:initform 0
	:reader last-account-number)))

(defmethod -next-account-number ((bank-account-repository bank-account-repository))
  (incf (slot-value bank-account-repository 'last-account-number)))

(defmethod -get-by-account-number ((bank-account-repository bank-account-repository) account-number)
  (find-if (lambda (account)
			 (= (getf account :account-number)
				account-number))
		   (bank-accounts bank-account-repository)))

(defmethod get-by-account-number ((bank-account-repository bank-account-repository) account-number)
  (let ((matching-account (-get-by-account-number bank-account-repository account-number)))
	(when matching-account
	  (deserialize bank-account-repository matching-account))))

(defmethod get-all ((bank-account-repository bank-account-repository))
  (let ((matching-accounts (bank-accounts bank-account-repository)))
	(when matching-accounts
	  (mapcar (lambda (account)
				(deserialize bank-account-repository account))
			  matching-accounts))))

(defmethod deserialize ((bank-account-repository bank-account-repository) account-plist)
  (make-instance 'bank-account
				 :account-number (getf account-plist :account-number)
				 :customer-name (getf account-plist :customer-name)
				 :balance (getf account-plist :balance)
				 :on-hold (getf account-plist :on-hold)))

(defmethod create ((bank-account-repository bank-account-repository) (bank-account bank-account))
  (setf (bank-accounts bank-account-repository)
		(cons (serialize bank-account-repository bank-account)
			  (bank-accounts bank-account-repository))))

(defmethod update ((bank-account-repository bank-account-repository) (bank-account bank-account)) 
  (setf (bank-accounts bank-account-repository)
		(mapcar (lambda (account-plist)
				  (if (= (getf account-plist :account-number)
						 (account-number bank-account))
					  (serialize bank-account-repository bank-account)
					  account-plist))
				(bank-accounts bank-account-repository))))

(defmethod serialize ((bank-account-repository bank-account-repository) (bank-account bank-account))
  (with-slots (account-number customer-name balance on-hold) bank-account
	(list :account-number (if (plusp account-number)
							  account-number
							  (-next-account-number bank-account-repository))
		  :customer-name customer-name
		  :balance balance
		  :on-hold on-hold)))

(defclass bank-account-service ()
  ((bank-account-repository
	:reader bank-account-repository)))

(defmethod initialize-instance :after ((bank-account-service bank-account-service) &key bank-account-repository)
  (setf (slot-value bank-account-service 'bank-account-repository)
		bank-account-repository))

(defmethod hold-account ((bank-account-service bank-account-service) account-number)
  (let ((account (get-by-account-number (bank-account-repository bank-account-service) account-number)))
	(when account
	  (setf (slot-value account 'on-hold) t)
	  (update (bank-account-repository bank-account-service) account))
	account))


(defun app-context ()
  (let* ((repositories (list :bank-account-repository (make-instance 'bank-account-repository)))		 
		 (services (list :bank-account-service (make-instance 'bank-account-service :bank-account-repository (getf repositories :bank-account-repository)))))
	(list :repositories repositories
		  :services services)))
