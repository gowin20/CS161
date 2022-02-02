;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

; George Owen
; 405196467

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; DFSRL: stands for Depth-First Search Right-Left, which is the purpose of this function:
; it traverses the input tree FRINGE and outputs a top-level list containing all of the tree's nodes

; input: FRINGE, a tree represented by a (possibly nested) list, or an atom if tree is a single element
; example input: ((((L) E) F) T)
; output: a non-nested list containing all of the tree's nodes from RIGHT to LEFT in DFS fashion.
; example output: (T F E L)
(defun DFSRL (FRINGE)
    (cond
    ; base case: end of list
    ((equal nil FRINGE)
        nil)
    ; if FRINGE is an atom, return it in list form so it works with append
    ((not (listp FRINGE))
        (list FRINGE))
    ; otherwise, FRINGE is always a list with items in it
    (t 
        (let 
            ; recursive calls on both the current item (could be a subtree) and the rest of the tree
            ((fst (DFSRL (car FRINGE)))
            (rst (DFSRL (cdr FRINGE))))
            ; fst = current element, rst = rest of tree. 
            ; put the output of rest of tree BEFORE current element, so that it properly reflects right to left DFS
            ; append strips sublists and creates one top-level list of leaf nodes
            (append rst fst)
        ))
    )
)

;(write (DFSRL `(ROOT) ))
;(write (DFSRL `((((L E) F) T)) ))
;(write (DFSRL `((R (I (G (H T))))) ))
;(write (DFSRL `(((A (B)) (D) C)) ))
;(write (DFSRL `((T (H R E) E)) ))
;(write (DFSRL `((A ((C ((E) D)) B))) ))

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;
 

; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.



; heading comments for these functions provided by professor.




; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (equal S (list T T T T)))

;(write (FINAL-STATE (list T T T T)))
;(write (FINAL-STATE `(T T T T)))
;(write (FINAL-STATE (list NIL T T T)))
;(write (FINAL-STATE `(NIL T T T)))
; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poison and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).

; this function contains all of the logic of the game
(defun NEXT-STATE (S A)
    (let 
        (   ;break down state into four variables
            (HOMER (first S))
            (BABY (second S))
            (DOG (third S))
            (POISON (fourth S))
        )
        (cond
            ; homer crosses alone
            ((equal A `h)
                ; always possible for homer to cross: check for invalid states
                (cond 
                    ;dog and baby left on same side, homer leaving
                    ((and (equal DOG BABY) (equal HOMER BABY)) 
                        NIL)
                    ;baby and poison left on same side, homer leaving
                    ((and (equal BABY POISON) (equal HOMER BABY))
                        NIL)
                    ;valid crossing
                    (t (list (list (not HOMER) BABY DOG POISON))))
                )
                
            ; homer crosses with baby (must be on same side)
            ((equal A `b)
                (cond 
                    ; make sure they're on the same side
                    ((equal HOMER BABY)
                        ; don't need to check any invalid states with BABY since homer will be with the baby
                        (list (list (not HOMER) (not BABY) DOG POISON)))
                    ; not on the same side: action is impossible, return NIL
                    (t NIL)
                )
            )
            ; homer crosses with dog
            ((equal A `d)
                (cond 
                    ; make sure they're on the same side
                    ((equal HOMER DOG)
                        ;check for invalid states: can ignore dog+baby since homer is with dog
                        (cond
                            ; if homer, baby, and poison are together, homer crossing will cause an invalid state
                            ((and (equal BABY POISON) (equal BABY HOMER))
                                NIL)
                            ; otherwise, make the crossing
                            (t
                                (list (list (not HOMER) BABY (not DOG) POISON))))
                        )
                        
                    ; not on the same side: action is impossible, return NIL
                    (t NIL)
                )
            )
            ; homer crosses with poison
            ((equal A `p)
                (cond 
                    ; make sure they're on the same side
                    ((equal HOMER POISON)
                        ;check for invalid states: can ignore baby+poison since homer is with poison
                        (cond
                            ; if homer, baby, and dog are together, homer crossing will cause an invalid state
                            ((and (equal BABY DOG) (equal BABY HOMER))
                                NIL)
                            ; otherwise, make the crossing
                            (t
                                (list (list (not HOMER) BABY DOG (not POISON)))))
                        )
                        
                    ; not on the same side: action is impossible, return NIL
                    (t NIL)
                )
            )
        
        )
    )
)
;(write (NEXT-STATE (list NIL NIL T T) `b))


; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
    ; could realistically do this function in one line but that's not my vibe
    (let
        (   ; call NEXT-STATE with each of the four possible moves
            (HOMER (NEXT-STATE S `h))
            (BABY (NEXT-STATE S `b))
            (DOG (NEXT-STATE S `d))
            (POISON (NEXT-STATE S `p))
        )
        ; appending all four together is all we need to do since (append) takes care of the rest
        ; illegal/invalid moves return NIL, which dissappears when appended with lists.
        (append HOMER BABY DOG POISON)
    )
)
;(write (SUCC-FN `(T T T T)))
;(write (append NIL (list `(a b c d)) (list `(e f g h))))
;(write (append (list `(a b c d)) (list `(e f g h))))


; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
    (cond 
        ; if we've searched the entire path, return NIL
        ((equal NIL STATES) NIL)
        (t
            (cond
                ; if this is the state we're looking for, return T
                ((equal S (car STATES)) T)
                ; otherwise, search the rest of the list
                (t (ON-PATH S (cdr STATES)))
        ))    
    )
)
;(write (ON-PATH (list T T T T) (list `(T NIL NIL T) `(NIL NIL NIL NIL) `(T NIL T T) `(T T T T))))

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
    ; recursively cycle through and attempt each of the successor states
    (cond
        ; base case: we've checked every possible next state and they are all nil
        ((equal NIL STATES) NIL)
        (t
            ; or is our friend :)
            (let 
                ; perform DFS on one possible state from the list
                ((thisPATH (DFS (car STATES) PATH)))
                (cond
                    ; we only want to search other states after confirming this path is not the solution
                    ((null thisPATH) 
                        (MULT-DFS (cdr STATES) PATH))
                    ; if this path IS a solution, return it immediately
                    (t thisPATH)
                )
            )
        )
    )
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as  well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
    (cond 
        ; if this is the goal state, return the path immediately!
        ((FINAL-STATE S) (append PATH (list S)))
        (t
            (cond
                ; if the state has been visited already, return immediately
                ((ON-PATH S PATH) NIL)
                ; otherwise it is a new state which isn't the goal state
                (t 
                    ; expand all possible options, generate a list of possible states
                    (let
                        ((STATES (SUCC-FN S)))

                        ; for each option, call DFS. add current node to PATH
                        (MULT-DFS STATES (append PATH (list S)))
                    )
                )
            )
        )
    )
)

(write (DFS `(NIL NIL NIL NIL) NIL))