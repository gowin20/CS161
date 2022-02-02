;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )


(defun isAnyBox (v)
	(or (= v box) (= v boxstar))
)

(defun isAnyKeeper (v)
	(or (= v keeper) (= v keeperstar))
)

(defun isAnyStar (v)
	(or (= v star) (= v boxstar) (= v keeperstar))
)

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

#|
 |
 |  MY FUNCTIONS
 |
 |
 |#



; goal-row: helper function for goal-test
; input: a list r representing a single row in a game state
; output: a boolean value t/nil indicating whether the row contains any boxes (box but not boxstar)
(defun goal-row (r)
	;cycle through a single row to see if it contains any boxes. if there are boxes, return NIL
	(cond
		((null r) t) ;reached end of row without finding any boxes
		(t 
			(if (isBox (car r)) ;if there is a box, return NIL
			nil
			(goal-row (cdr r)) ;otherwise check the rest
			) ;end if
		) ;end t condition
	) ;end cond
);end goal-col



; goal-test: checks if a game of sokoban is in a valid goal state
; a state is a goal state if there are no misplaced boxes, i.e. the only boxes present in the game are on top of stars, so they have a value of "boxstar" instead of "box"
; input: a valid game state s
; output: a boolean value t/nil indicating whetehr the state is a valid goal state
(defun goal-test (s)
	(cond
		((null s) t) ;reached the end of game without finding any boxes
		(t 
			(if (goal-row (car s)) ;if there are no boxes in a row
				(goal-test (cdr s)) ;check the rest of the rows
				nil ;otherwise there is a box and that sucks man.
			) ;end if
		) ;end t condition
	) ;end cond
);end defun



; COORDINATE REFERENCE: index begins in the top left at (0,0)



; get-col: helper function for get-square
; input: a list ROW representing a single row of a valid game state, an integer c indicating an index of the list
; returns the item at index "c" of a list, representing the item in the cth column of the board at a specific row. NIL if invalid request.
; returns NIL on 
(defun get-col (ROW c)
	(cond
		((null ROW) nil) ; if the end of list is reached before c = 0, given index is out of bounds
		((= c 0) (car ROW)) ; if we've arrived at the index, return the item's value
		(t (get-col (cdr ROW) (- c 1))) ; search the rest of list, and decrement c
	) ; end cond
) ; end function



; get-square: finds the value of an item in the state
; input: a valid game state STATE, integers r and c indicating a coordinate in the state.
; output: value of the item at row 'r', column 'c' of a state when indexing from (0,0) at the top left.
; this function finds the row, then finds the item in the column by calling get-col. returns NIL on invalid request
(defun get-square (STATE r c)
	(cond
		((null STATE) nil) ; nil on invalid request
		((= r 0) (get-col (car STATE) c)) ; once row is found, return item
		(t (get-square (cdr STATE) (- r 1) c)) ;keep cycling through until you get to the row
	) ; end cond
) ; end function



; set-item - helper function for set-square
; input: a list ROW, a column value c, and a value v to set the state to
; output: a new list identical to ROW except the item at index c is set equal to v
; if input is out of bounds, ROW is returned unmodified
(defun set-item (ROW c v)
	(cond
		((null ROW) nil) ;end of list
		((= c 0) (append (list v) (set-item (cdr ROW) (- c 1) v))) ;modify the item when found
		(t (append (list (car ROW)) (set-item (cdr ROW) (- c 1) v))) ;otherwise add the item unmodified
	) ; end cond
) ; end function



; set-square - helper function used by many other functions
; input: a state STATE, row and column values r and c, and a value v to set the state to.
; output: a new state identical to STATE, except the square at coordinate (r,c) is set equal to v
; if input is out of bounds, STATE is returned unmodified.
(defun set-square (STATE r c v)
	(cond
		((null STATE) nil) ; end of state
		((= r 0) (append (list (set-item (car STATE) c v)) (set-square (cdr STATE) (- r 1) c v))) ; if this is the row, set the item and add to output
		(t 		 (append (list (car STATE)) (set-square (cdr STATE) (- r 1) c v))) ; if this isn't the row, simply add the list to output
	) ; end cond
) ; end function



; getXAway: gets coordinates in a direction (deals with directional input)
; input: spatial coordinates: row r, column c. a direction symbol D, and a distance X
; output: the set of coordinates X units away from (r,c) in direction D
; ex: (getXAway 0 0 `RIGHT 2) returns (0 2), (getXAway 0 0 `DOWN 1) returns (1 0)
; used to find the coordinates of squares 1 and 2 units away from the keeper
(defun getXAway (r c D X)
	(cond
		((equal D `UP)
			(list (- r X) c) ;up: subtract from row
		)
		((equal D `DOWN)
			(list (+ r X) c) ;down: add to row
		)
		((equal D `LEFT)
			(list r (- c x)) ;left: subtract from col
		)
		((equal D `RIGHT)
			(list r (+ c x)) ;right: add to col
		)
		(t nil) ;invalid direction input
	)
) ; end function



; try-move: tries to move the keeper one unit in direction D and generate resulting state.
; input: a state STATE and direction symbol D (`UP, `DOWN, `LEFT, `RIGHT)
; output: the state resulting from moving the keeper in STATE one square in direction D, or NIL if that is not a valid move.
(defun try-move (STATE D)
	;get a bunch of necessary variables and coordinates at the start
	(let* 
		; get keeper position
		((pos (getKeeperPosition STATE 0))
		(c (car pos))
		(r (cadr pos))
		;r and c are now the coordinate of the keeper in s.
		; get coordinates of item 1 square away
		(away1 (getXAway r c D 1))
		(r1 (car away1))
		(c1 (cadr away1))
		;r1 and c1 are now the coordinates of the square 1 unit away in direction D
		; get coordinates of item 2 squares away
		(away2 (getXaway r c D 2))
		(r2 (car away2))
		(c2 (cadr away2))
		;r2 and c2 are now the coordinates of the square 2 units away in direction D
		(item1 (get-square STATE r1 c1))
		; item1 is the value of the square 1 unit away in direction D
		(item2 (get-square STATE r2 c2))
		; item2 is the value of the square 2 units away in direction D
		) ;end declarations
		; now, check for invalid moves:
		(cond
			((null item1) NIL) ;can't walk out of bounds
			((isWall item1) NIL) ;can't walk into a wall

			((and (isAnyBox item1) (null item2)) NIL) ;can't push a box out of bounds
			((and (isAnyBox item1) (isWall item2)) NIL) ;can't push a box into a wall
			((and (isAnyBox item1) (isAnyBox item2)) NIL) ;can't push a box into a box

			; if it's not NIL by now, then this is a legal move
			(t 
				(let* 
				;here I use let* to sequentially "modify" STATE three times, since we're not allowed to use setq. 
				; the first modification moves a box if there is one, the second moves keeper, the third cleans up keeper's original position
					(
						(STATE-MOD1 
						; first modification of state. move box if there is one, otherwise STATE-MOD1 will equal STATE
							(if (isAnyBox item1) 
									(if (isAnyStar item2) ; if destination is a star
										(set-square STATE r2 c2 boxstar) ;set star to boxstar
										(set-square STATE r2 c2 box) ; otherwise set box destination to box
									) ;end if
									
									STATE ;Else: just set it to be state
							) ;end if
						) ;end STATE-MOD1
						(STATE-MOD2
						; second modification of state. moves the keeper to the proper square
							(if (isAnyStar item1) ;if keeper destination is a star
								(set-square STATE-MOD1 r1 c1 keeperstar) ;set it to keeper on top of star
								(set-square STATE-MOD1 r1 c1 keeper) ;otherwise, set it to just keeper
							) ;end if
						) ;end STATE-MOD2
						(STATE-FINAL
						; final modification of state. ;revert original keeper position back to what it was before
							(if (isKeeperStar (get-square STATE r c)) ;if keeper was standing on a star
								(set-square STATE-MOD2 r c star) ;set position back to a star
								(set-square STATE-MOD2 r c blank) ;otherwise, make where the keeper was standing empty again
							)
						) ;end STATE-FINAL
					) ;end declarations

					STATE-FINAL ; RETURN:  final modified state

				) ;end let*
			) ;end t condition
		) ;end cond

	) ; end let
) ; end function


; next-states: generates all possible next game states based on a given state. crucial function for A* Search
; input: a game state STATE
; output: a list of game states, between 0 and 4 based on the number of possible valid game moves.
; basically try to move in each direction and generate the resulting state, and put them all into a list
(defun next-states (STATE)
  (let*
	 ((result (list (try-move STATE `UP) (try-move STATE `DOWN) (try-move STATE `LEFT) (try-move STATE `RIGHT)))) ;try to move in each direction, lump into list
    (cleanUpList result);return and remove any NILs resulting from invalid moves
  );end let
);end defun



; ----------------------------------------
;			HEURISTICS
; ----------------------------------------


; h0: trivial admissible heuristic function
; input: a state S (not actually used in function)
; output: 0
(defun h0 (s) 0)


; ------------------------------
; 			H1
; ------------------------------

; count-box-row: counts the number of misplaced boxes in a row. Helper function for h1
; input: a row r, represented as list
; output: the number of misplaced boxes in the row
(defun count-box-row (r)
	(cond
		((null r) 0) ; base case
		((isBox (car r)) (+ 1 (count-box-row (cdr r)))) ; if there's a box, add one to the count
		(t (count-box-row (cdr r))) ; otherwise keep counting without adding
	)
)

; h1: heuristic which counts the number of misplaced boxes (boxes not on stars) in a given state s
; input: a state S
; output: the number of misplaces boxes in the entire state.

;
; ARGUMENT FOR ADMISSIBILITY: 
; H1 is an admissible heuristic as each misplaced box will take at least one move to place correctly.
; even in the case where every misplaced box takes exactly 1 move to fix, and every move made places a box onto a goal, 
;		the output of H1 will equal the amount of moves remaining and never exceed it.
; Thus H1 is admissible as it will always be less than or equal to the actual amount of moves remaining in the optimal solution

; This is the Hamming Distance of a sokoban puzzle.

(defun h1 (s)
	(cond
		((null s) 0) ; base case
		(t (+ (count-box-row (car s)) (h1 (cdr s)))) ; count the current row, and add it to the total count
	)
)

; ------------------------------
; 			H405196467
; ------------------------------

; H405196467 calculates the Manhattan Distance from each box to its closest goal.
; multiple boxes may have the same closest goal, and we are assuming that no obstacles such as walls or other boxes exist.

; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.


;;;;----
; 
; H405196467 helper functions
;
; FUNCALL would be nice here :'(
; original goal: generalize all-box-row/all-star-row to "all-x-row", all-box-locations/all-star-locations to "all-x-locations"
; this higher-order programming is not possible in LISP without the use of funcall or other disallowed functions, which makes me sad :(
; 
;;;;----


; all-box-row
; return list of coordinates of every box in the row
; input: a row ROW, coordinate trackers r and c
; output: a list of coordinates for all of the boxes in the row
(defun all-box-row (ROW r c)
	(cond
		((null ROW) nil) ;base case
		((isAnyBox (car ROW)) (append (list (list r c)) (all-box-row (cdr ROW) r (+ 1 c)))) ;if is star, add coordinates to list
		(t (all-box-row (cdr ROW) r (+ 1 c))) ; otherwise keep searching
	)
) ;end function

; all-box-row
; return list of coordinates of every star in the row
; input: a row ROW, coordinate trackers r and c
; output: a list of coordinates for all of the stars in the row
(defun all-star-row (ROW r c)
	(cond
		((null ROW) nil) ;base case
		((isAnyStar (car ROW)) (append (list (list r c)) (all-star-row (cdr ROW) r (+ 1 c)))) ;if is star, add coordinates to list
		(t (all-star-row (cdr ROW) r (+ 1 c))) ; otherwise keep searching
	)
) ;end function

; return list of coordinates of every box in a given state
; input: a state s
; output: a list of coordinate pairs containing the locations of all boxes in the state
(defun all-box-locations (STATE r c)
	(cond
		((null STATE) nil) ; base case
		(t (append (all-box-row (car STATE) r c) (all-box-locations (cdr STATE) (+ r 1) c))) ;get all the goals
	)
) ;end function

; return list of coordinates of every star in a given state
; input: a state s
; output: a list of coordinate pairs containing the locations of all stars in the state
(defun all-star-locations (STATE r c)
	(cond
		((null STATE) nil) ; base case
		(t (append (all-star-row (car STATE) r c) (all-star-locations (cdr STATE) (+ r 1) c))) ;get all the goals
	)
) ;end function

; all-stars: wrapper for all-x-locations
; input: a state
; output: a list of coordinate pairs for all boxes in a given state
(defun all-boxes (STATE)
	(all-box-locations STATE 0 0)
)

; all-stars: wrapper for all-x-locations
; input: a state
; output: a list of coordinate pairs for all stars in a given state
(defun all-stars (STATE)
	(all-star-locations STATE 0 0)
)


; shortest-distance: for a given box coordinate, find the distance between the box and the closest goal location
; helper function for total-distance
; input: a list of star coordinates STARS, a box coordinate BOX, and a distance tracker distance (initially NIL)
; output: distance, the amount of squares between the box and the closest goal location
(defun shortest-distance (STARS BOX distance)
	(cond
		((null STARS) distance) ;base case, return distance found
		(t
			; calculate manhattan distance
			(let*
				((boxR (car BOX))
				(boxC (cadr BOX))
				(starR (caar STARS))
				(starC (cadar STARS))
				(manhattanD (+ (abs (- boxR starR)) (abs (- boxC starC))))) ;calculate the distance
				(cond
					((or (null distance) (> manhattanD distance)) ;if the calculated distance is the shortest one so far, or the first one
						(shortest-distance (cdr STARS) BOX manhattanD) ;replace with new distance 
					)
					(t (shortest-distance (cdr STARS) BOX distance)) ;otherwise keep the original distance
				) ;end cond
			) ;end let
		) ;end t condition
	) ;end cond
) ;end function


; total-distance: for each box, find the distance between box and the closest goal location to it. return the sum of all of them, which is an admissible heuristic
; input: a list of star coordinates STARS, a list of box coordinates BOXES
; output: a number representing the summed total distances between each box and the closest goal to that box.
(defun total-distance (STARS BOXES)
	(cond
		((null BOXES) 0) ;base case
		(t
			(+ (shortest-distance STARS (car BOXES) NIL) (total-distance STARS (cdr BOXES))) ;find shortest distance for one box, recursively search the rest
		) ;end t condition
	) ;end cond
) ;end function


;
; MY HEURISTIC: H405196467
; input: a valid state S
; output: the summed manhattan distance between each box and its closest goal location

; ARGUMENT FOR ADMISSIBILITY:
; The manhattan distance shows how many tiles away a box is from the closest goal location on the grid.
; in order to solve a puzzle, every misplaced box must be moved at least that many tiles in order to reach a goal location, as that is the closest goal
; in reality, the manhattan distance will certainly underestimate the majority of puzzles. boxes are not always moved to their closest goal as only one box may occupy each star position
; additionally, this heuristic discounts walls and other obstacles, as well as moves which do not push boxes and simply reposition the player
; in conclusion, this heuristic is certainly admissible as it will always estimate a number of moves less than or equal to the number in an optimal solution.

(defun h405196467 (s)
	(let*
		((BOXES (all-boxes s)) ;get all boxes
		(STARS (all-stars s)) ;get all stars
		(distance (total-distance STARS BOXES)) ;calculate total shortest manhattan distance between all boxes and their closest stars
		)
		distance ;return summed distance as heuristic
	) ;end let
) ;end function



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#


;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
