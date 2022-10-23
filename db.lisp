;;;; CD Database

(defvar *db* nil
  "Database property list")

(defun make-cd (title artist rating is-ripped)
  (list :title title
	:artist artist
	:rating rating
	:is-ripped is-ripped))

(defun add-record (record)
  (push record *db*))

;;(add-record (make-cd "Tubular Bells" "Mike Oldfield" 3 nil))
;;(add-record (make-cd "Ten Thousand Fists" "Disturbed" 4 t))

(defun dump-db ()
  "Returns contents of *DB* in a readable format using REDUCE and manual printing"
  (reduce (lambda (string record)
	    (format nil "~A~%~%~A" string 
		    (let* ((title (getf record :title))
			   (artist (getf record :artist))
			   (rating (getf record :rating))
			   (is-ripped (getf record :is-ripped))
			   (is-ripped-text (if is-ripped
					       "Ripped"
					       "Not Ripped")))
		      (format nil "TITLE: ~A~%ARTIST: ~A~%RATING: ~A/5~%RIPPED: ~A"
			      title artist rating is-ripped-text))))
	  *db*
	  :initial-value ""))

(defun dump-db-a ()
  "Returns contents of *DB* in a readable format using FORMAT functionality"
  (reduce (lambda (string record)
	    (concatenate 'string
			 string
			 ;; ~{ ~} allows FORMAT to iterate over a list
			 (format nil "~{~a:~10t~a~%~}~%" record)))
	  *db*
	  :initial-value ""))

(defun prompt-read (prompt)
  "Prompts the user with PROMPT, returning the user's input"
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd (prompt-read "Title")
	   (prompt-read "Artist")
	   (or (parse-integer (prompt-read "Rating")
			      :junk-allowed t)
	       0)
	   (y-or-n-p "Is Ripped?")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
	(when (not (y-or-n-p "Add another?"))
	  (return))))

(defun save-db (filename)
  ;; OUT is bound by WITH-OPEN-FILE to an outstream for writing to a file
  (with-open-file (out
		   filename
		   :direction :output
		   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in
		   filename
		   :direction :input)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select-by-artist (artist)
  (flet ((matching-artist (record)
	   (equal (getf record :artist)
		  artist)))
    (remove-if-not #'matching-artist
		   *db*)))

(defun select (selector)
  ;; SELECTOR is not prefixed with #' as
  ;; it is a variable whose value is a function
  (remove-if-not selector *db*))

(defun artist-selector (artist)
  (lambda (record)
    (equal (getf record :artist) artist)))

;; (where :artist "something") => (artist-selector "something")
(defun where-with-flet (&key title artist rating (is-ripped nil is-ripped-p))
  (flet ((where-key (key value record)
           (equal (getf record key)
		  value)))
    (lambda (record)
      (and (or (null title)
	       (where-key :title title record))
	   (or (null artist)
	       (where-key :artist artist record))
	   (or (null rating)
	       (where-key :rating rating record))
	   (or (null is-ripped-p)
	       (where-key :is-ripped is-ripped record))))))

(defun where (&key title artist rating (is-ripped nil is-ripped-p))
  (lambda (record)
    (and (or (null title)
             (equal (getf record :title)
		    title))
	 (or (null artist)
             (equal (getf record :artist)
		    artist))
	 (or (null rating)
             (equal (getf record :rating)
		    rating))
	 (or (null is-ripped-p)
             (equal (getf record :is-ripped)
		    is-ripped)))))

;; (update (where ...) :k v)
(defun update (selector &key title artist rating (is-ripped nil is-ripped-p))
  "Updates rows retrieved with SELECTOR according to KEY arguments"
  ;; Alternative implementation has only 1 loop
  (let ((records (select selector)))
    (mapcar (lambda (record)
	      (and (or (null title)
		       (setf (getf record :title)
			     title))
		   (or (null artist)
		       (setf (getf record :artist)
			     artist))
		   (or (null rating)
		       (setf (getf record :rating)
			     rating))
		   (or (null is-ripped-p)
		       (setf (getf record :is-ripped)
			     is-ripped)))
	      record)
	    records)))

(defun delete-rows (selector)
  (setf *db* (remove-if selector
			*db*)))

;; ----
;; Using macros
;; ----

;; (where :title "fred") -> (lambda (record) (equal (getf record :title) "fred"))
;; Consider way to generalise arguments
;; Amounts to much the same (ended up a function instead of macro)
(defun defwhere1 (&key title artist rating (is-ripped nil is-ripped-p))
  (let ((new-lambda-sym `(lambda (record)
			   (and ,(or (null title)
				     (list 'equal '(getf record :title) title))
				,(or (null artist)
				     (list 'equal '(getf record :artist) artist))
				,(or (null rating)
				     (list 'equal '(getf record :rating) rating))
				,(or (null is-ripped-p)
				     (list 'equal '(getf record :is-ripped) is-ripped))))))
    (format t "~A" new-lambda-sym)
    new-lambda-sym))

(defun where-allow-other-keys (&rest args &key &allow-other-keys)
  "WHERE using &ALLOW-OTHER-KEYS."
  (lambda (record)
    (not (loop for (key value) on args
	       if (symbolp key)
	       do (when (not (equal (getf record key)
				    value))
		    (return t))))))

;; ---------------

(defun make-comparison-expr (field value)
  `(equal (getf record ,field)
	  ,value))

;; Book expects a list here instead, but &KEY &ALLOW-OTHER-KEYS ensures correct
;; argument structure
(defun make-comparisons-list (&rest fields &key &allow-other-keys)
  (loop while fields
	collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro defwhere (&rest clauses &key &allow-other-keys)
  `(lambda (record)
     (and ,@(apply #'make-comparisons-list clauses))))
