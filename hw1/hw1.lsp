; George Owen
; 405196467
; hw1.lsp
; all free "write" statements and function calls removed for submission.

; SEQ function: calculates tribonacci numbers
; argument N: specifies which tribonacci number to calculate
; output: the Mth tribonacci number
(defun SEQ (N) (
    ; fibonacci with 3: tribonacci
    cond 
    ((or (equal N 0) (equal N 1) (equal N 2)) 
        1)
    (t 
        (+ (SEQ (- N 1)) (SEQ (- N 2)) (SEQ (- N 3))))

))

; test-SEQ: test function for SEQ
; argument TIMES: specifies how many times to call "SEQ" in descending order
;  output: values of SEQ from 0 to TIMES
(defun test-SEQ (TIMES) (
    cond ((equal TIMES 0) 1)
    (t 
    ;recursively calls "TIMES" times
        (write (SEQ TIMES))
        (write-line "")
        (test-SEQ (- TIMES 1)))
))


; SUMS function: counts the number of arithmetic operations performed in SEQ
; argument N: represents the Nth iteration of SEQ, which calculates the Nth tribonacci number
; output: the number of arithmetic operations performed when calling SEQ(N)
(defun SUMS (N) (
    cond 
    ;each call of SEQ contains two addition operators, except for SEQ 0, SEQ 1, and SEQ 2 which contain 0
    ((or (equal N 0) (equal N 1) (equal N 2)) 
        0)
    (t 
        (+ 2 (SUMS (- N 1)) (SUMS (- N 2)) (SUMS (- N 3))))
    ; functionally, (SUMS n) equates to (- (SUMS n) 1)
))

; test-SUMS: test function for SUMS
; argument TIMES: specifies how many times to call "SUMS" in descending order
; output: values of SUMS from 0 to TIMES
(defun test-SUMS (TIMES) (
    cond 
    ((equal TIMES 0) 
        (write 0))
    (t 
    ;recursively calls "TIMES" times
        (write (SUMS TIMES))
        (write-line "")
        (test-SUMS (- TIMES 1)))
))




; ANON function: anonymizes an input tree (represented as a list)
; argument TREE: represents a tree containing data in the form of either a list or single atom
; output: an anonomized version of TREE, maintaining the same data structure but replacing all atoms with the value 0.
(defun ANON (TREE) (
    cond
    ; replace all atoms with 0
    ((not (listp TREE)) 
        0)
    ; base case for end of lists
    ((equal nil TREE) nil)
    ; in all other cases, is a list with items contained
    (t
        (let 
        ((fst (car TREE))
        (rst (cdr TREE)))
        ;recursive call
        (cons (ANON fst) (ANON rst))
        ))
))