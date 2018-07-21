;decide if every element in a given list is nil
(defun all-nil-list-p (lst)
	(every (lambda (ele) (equal ele nil)) lst))

;decide if mat is a valid matrix
(defun matrix-p (mat)
	(apply #'= (mapcar #'list-length mat)))

;decide if matrices are conformational
(defun conf-mats-p (mat1 mat2)
	;check if matrices are valid
	(when (not (and (matrix-p mat1) (matrix-p mat2))) (return-from conf-mats-p nil))
	;do it!
	(= (list-length (car mat1)) (list-length mat2)))

;return transpost of given matrix
(defun calc-transpose (mat)
	;check if matrix is valid
	(when (not (matrix-p mat)) (return-from calc-transpose nil))
	;do it!
	(let ((rst (mapcar #'cdr mat)))
		(if (not (all-nil-list-p (mapcar #'car mat)))
			(cons (mapcar #'car mat) (calc-transpose rst))
			(eval nil))))

;multiply matrices
(defun matrix-mult (mat1 mat2)
	;check if matrices are conformational
	(when (not (conf-mats-p mat1 mat2)) (return-from matrix-mult nil))
	(format t "~a~%~a~%~%" mat1 mat2)
	;do it!
	(let ((res_mat nil))
		(labels ((row-col-mult (row col)
					(if (and row col) 
						(+ (row-col-mult (cdr row) (cdr col)) (* (car row) (car col)))
						(return-from row-col-mult 0)))
				(row-cols-mult (row cols)
					(mapcar (lambda (c) (row-col-mult row c)) cols)))
			(mapcar (lambda (r) (row-cols-mult r (calc-transpose mat2))) mat1))))
			
;multiply multiple matrices
(defun multi-matrix-mult (mats)
	(cond
		((equal (cdr mats) nil) (car mats))
		((not (conf-mats-p (car mats) (cadr mats))) (car mats))
		(t (matrix-mult (multi-matrix-mult (cdr mats)) (car mats)))))
		
;MAIN BODY
(defvar *mats* '(

				((1 1 0 0)
				 (1 0 0 0)
				 (0 0 0 1)
				 (0 0 2 1))
				 
				((0  2 0  0)        
				 (2  -2 0  0)
				 (0  0 -1 1)
				 (0  0 2  0))))
				 
(print (multi-matrix-mult *mats*))