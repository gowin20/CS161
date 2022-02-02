;
; Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw4.lsp")
  );end defun

; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
; conversion:
; the variable which says "node n gets color c, out of k possible colors"
; will have index (n-1)*k + c
(defun node2var (n c k)
  (+ c (* k (- n 1)))
  )

; returns *a clause* representing the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
;
; in practice, recursively calls node2var for values c .. k and returns a top-level list of all results, representing a disjunction of all variables
; (c v c+1 v c+2 v ... v k)
(defun at-least-one-color (n c k)
  (cond 
    ((> c k) nil) ;base case - we've added every variable index
    (t ; otherwise, add the next disjunction to constraint
      (append (list (node2var n c k)) (at-least-one-color n (+ c 1) k)) ; get variable index using node2var and recursively call the rest
    )  ; end t condition
  ) ; end cond
) ; end function


; DNF Form: (c ^ ~c+1 ^ ~c+2 ^ ... ^ ~k) v (~c ^ c+1 ^ ~c+2 ^ ... ^ ~k) v ... v (~c ^ ~c+1 ^ ~c+2 ^ ... ^ k)
; plan: explicitly require that every pair of variables have at least one false variable
; CNF Form: (~c v ~c+1) ^ (~c v ~c+2) ^ ... ^ (~c v ~k) ^ (~c+1 v ~c+2) ^ ... ^ (~c+1 v ~k) ^ ... ^ (~k-1 v ~k)
; outputs (k-c choose 2) clauses, or about (k-c)^2/2


; pair-clause:
; helper function for at-most-one-color
(defun pair-clause (n c c2 k)
  (cond
    ((> c2 k) nil)
    (t
      ;create a list of clauses of the form (~c v ~c') for all c' from c+1 -> k
      ; functionally looks like ((~c v ~c+1) ^ (~c v ~c+2) ^ ... ^ (~c v ~k))
      (append (list (list (* -1 (node2var n c k)) (* -1   (node2var n c2 k)))) (pair-clause n c (+ c2 1) k))
    ) ; end t condition
  ) ; end cond
) ; end function

; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
; accomplishes the formula in CNF by ensuring that NO TWO VARIABLES ARE TRUE
; since it's "at most one", the situation where zero variables are true is also acceptable here
(defun at-most-one-color (n c k)
  (cond
    ((= c k) nil) ; 
    (t
      (append (pair-clause n c (+ c 1) k) (at-most-one-color n (+ c 1) k)) ; call pair clause for all c, c+1 from 1 to k
    ) ; end t condition
  ) ; end cond
) ; end function

; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
; accomplishes the "exactly one" formula in CNF using two components:
; 1. one of the variables must be true (at-least-one)
; 2. no two of the variables can be true (at-most-one)
(defun generate-node-clauses (n k)
    (append (list (at-least-one-color n 1 k)) (at-most-one-color n 1 k)) ; uses at-least-one-color for (1.) and all-pair-clauses for (2.)
  )


; xy-pair-clause
; for each color c, can't have both (node2var x c k) and (node2var y c k) true
; either ONE or NEITHER of them ==> (~x v ~y) again
; (~xc v ~yc) ^ (~xc+1 v ~yc+1) ^ ... ^ (~xk v ~yk)
(defun xy-pair-clause (x y c k)
  (cond
    ((> c k) nil)
    (t
      (append (list (list (* -1 (node2var x c k)) (* -1 (node2var y c k)))) (xy-pair-clause x y (+ 1 c) k))
    ) ;end t cond
  )
) ; end defun

; generate-edge-clauses: functionally a wrapper for "xy-pair-clause"
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
(defun generate-edge-clauses (e k)
  (let
    ((x (car e))
    (y (cadr e)))
    (xy-pair-clause x y 1 k)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun
