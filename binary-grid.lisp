;;;; Based on Binary Grid

(defconstant +0+ #\0)
(defconstant +1+ #\1)
(defconstant +.+ #\.)

(defun inverse (value)
  (cond ((eq value +0+)
	 +1+)
	((eq value +1+)
	 +0+)
	(t
	 +.+)))

(defun grid-size (string)
  "Calculates the size of a grid using characters in STRING"
  (let ((sqrt-size (truncate (sqrt (length string)))))
    (if (evenp sqrt-size)
	sqrt-size
	0)))

(defun string->row (string size row)
  "Generates the ROWth grid row of size SIZE from STRING"
  (let ((start (* row size))
	(end (* (1+ row) size)))
    (loop for el across (subseq string start end)
	  collect el)))

(defun string->grid (string)
  "Generates a list of lists of square size, populated with characters in STRING"
  (let* ((size (grid-size string))
	(grid  (loop for row from 0 below size
		     collect (string->row string size row))))
    (list :grid grid :size size)))

(defun print-grid (grid)
  (format t "~{~A~%~}" (getf grid :grid)))

(defun grid->string (grid)
  (let ((grid-string "")
	(size (getf grid :size)))
    (loop for row from 0 below size
	  collect (loop for col from 0 below size
			do (setf grid-string
				 (format nil "~A~A"
					 grid-string (cell row col grid)))))
    grid-string))

(defun cell (row col grid)
  (let ((grid-list (getf grid :grid)))
    (nth col (nth row grid-list))))

(defun (setf cell) (new-value row col grid)
  (setf (nth col (nth row (getf grid :grid)))
	new-value))

;;; RULES

(defun oxo-col (row col grid)
  "Attempts to apply the oxo rule to GRID on cells (ROW, COL) and (ROW + 2, COL)"
  (cond ((eq +.+ (cell row col grid))
	 nil)
	((null (eq +.+ (cell (1+ row) col grid)))
	 nil)
	((> row (1- (getf grid :size)))
	 nil)
	((null (eq (cell row col grid)
		   (cell (+ 2 row) col grid)))
	 nil)
	(t
         (setf (cell (1+ row) col grid) (inverse (cell row col grid)))
	 t)))

(defun oxo-row (row col grid)
  "Attempts to apply the oxo rule to GRID on cells (ROW, COL) and (ROW, COL + 2)"
  (cond ((eq +.+ (cell row col grid))
	 nil)
	((null (eq +.+ (cell row (1+ col) grid)))
	 nil)
	((> row (1- (getf grid :size)))
	 nil)
	((null (eq (cell row col grid)
		   (cell row (+ 2 col) grid)))
	 nil)
	(t
         (setf (cell row (1+ col) grid) (inverse (cell row col grid)))
	 t)))

(defun pair-col (row col grid)
  "Attempts to apply the pair rule to GRID using cells (ROW, COL) and (ROW, COL + 1)"
  (cond ((eq +.+ (cell row col grid))
	 nil)
	((null (eq +.+ (cell (+ 2 row) col grid)))
	 nil)
	((> row (1- (getf grid :size)))
	 nil)
	((null (eq (cell row col grid)
		   (cell (1+ row) col grid)))
	 nil)
	(t
         (setf (cell (+ 2 row) col grid) (inverse (cell row col grid)))
	 t)))

(defun pair-row (row col grid)
  "Attempts to apply the pair rule to GRID using cells (ROW, COL) and (ROW + 1, COL)"
  (cond ((eq +.+ (cell row col grid))
	 nil)
	((null (eq +.+ (cell row (+ 2 col) grid)))
	 nil)
	((> col (1- (getf grid :size)))
	 nil)
	((null (eq (cell row col grid)
		   (cell row (1+ col) grid)))
	 nil)
	(t
         (setf (cell row (+ 2 col) grid)
	       (inverse (cell row col grid)))
	 t)))

(defun count-in-row (item row grid)
  (let ((grid-list (getf grid :grid)))
    (count item (nth row grid-list))))

(defun set-in-row (value row grid)
  (let ((grid-size (getf grid :size)))
    (loop for col from 0 below grid-size
	  do (when (eq +.+ (cell row col grid))
	       (setf (cell row col grid) value)))))

(defun parity-row (row grid)
  (let* ((grid-size (getf grid :size)) 
	 (1-count (count-in-row +1+ row grid))
	 (0-count (count-in-row +0+ row grid))
	 (.-count (count-in-row +.+ row grid)))
    (cond ((zerop .-count)
	   nil)
	  ((= 0-count (/ grid-size 2))
	   (set-in-row +1+ row grid)
	   t)
	  ((= 1-count (/ grid-size 2))
	   (set-in-row +0+ row grid)
	   t))))

(defun count-in-col (item col grid)
  (let* ((grid-list (getf grid :grid))
	 (col-values (loop for row in grid-list
			   collect (nth col row))))
    (count item col-values)))

(defun set-in-col (value col grid)
  (let ((grid-size (getf grid :size)))
    (loop for row from 0 below grid-size
	  do (when (eq +.+ (cell row col grid))
	       (setf (cell row col grid) value)))))

(defun parity-col (col grid)
  (let* ((grid-size (getf grid :size)) 
	 (1-count (count-in-col +1+ col grid))
	 (0-count (count-in-col +0+ col grid))
	 (.-count (count-in-col +.+ col grid)))
    (cond ((zerop .-count)
	   nil)
	  ((= 0-count (/ grid-size 2))
	   (set-in-col +1+ col grid)
	   t)
	  ((= 1-count (/ grid-size 2))
	   (set-in-col +0+ col grid)
	   t))))

;;; SOLVING

(defun solve-cell (row col grid)
  (find t (list (oxo-row row col grid)
		(oxo-col row col grid)
		(pair-col row col grid)
		(pair-row row col grid)
		(parity-row row grid)
		(parity-col col grid))))

(defun solve-grid-cells (grid)
  (let ((size (getf grid :size)))
    (find t (loop for row from 0 below size
		  collect (find t (loop for col from 0 below size
					collect (solve-cell row col grid)))))))

(defun solve (grid)
  (loop for change-made = (solve-grid-cells grid)
	until (null change-made)))

(defun solved-p (grid)
  (let ((size (getf grid :size)))
    (null (find t (loop for row from 0 below size
			collect (find t (loop for col from 0 below size
					      collect (eq +.+
							  (cell row col grid)))))))))
