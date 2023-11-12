(defvar *cannibals* '(C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 C11 C12 C13 C14 C15 C16 C17 C18 C19 C20 C21))
(defvar *missionaries* '(M1 M2 M3 M4 M5 M6 M7 M8 M9 M10 M11 M12 M13 M14 M15 M16 M17 M18 M19 M20 M21))

(defun run ()
	"run the missionary cannibal solveur "
  (prog (initial goal boat-cap)
		(format t "~%generate solution for :~% 15 cannibals, and 15 missionaries and one boat with capacity equal to 6. ~%")
		
		;; "set the initial state and the goal state"		
		(setq initial '((M1 M2 M3 M4 M5 M6 M7 M8 M9 M10 M11 M12 M13 M14 M15 C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 C11 C12 C13 C14 C15 B) ()))
		(setq goal '(() (M1 M2 M3 M4 M5 M6 M7 M8 M9 M10 M11 M12 M13 M14 M15 C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 C11 C12 C13 C14 C15 B)))
		(setq boat-cap 6)

		(let ( (solution (mis-can-solve (list (list initial (heuristic initial goal) nil)) nil goal boat-cap)))
		  (cond
			((state-eq (caar solution) goal)
			 (format t "SOLUTION GENERATED. THE PATH TO THE GOAL IS DISPLAYED. ~%")
			 (dolist (x (rebuild-path solution (car solution))) (print x))
			 (format t " ~%")
			 )
			(t
			 (format t "THERE IS NO SOLUTION. THE FOLLOWING (STATE HEURISTIC PARENT) TRIPLES WERE GENERATED. ~%")
			 (dolist (x solution) (print x))))))

	(prog (initial goal boat-cap)
		(format t "~%~%generate solution for :~% 20 cannibals, and 20 missionaries and one boat with capacity equal to 6. ~%")
		
		(setq initial '((M1 M2 M3 M4 M5 M6 M7 M8 M9 M10 M11 M12 M13 M14 M15 M16 M17 M18 M19 M20 C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 C11 C12 C13 C14 C15 C16 C17 C18 C19 C20 B) ()))
		(setq goal '(() (M1 M2 M3 M4 M5 M6 M7 M8 M9 M10 M11 M12 M13 M14 M15 M16 M17 M18 M19 M20 C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 C11 C12 C13 C14 C15 C16 C17 C18 C19 C20 B)))
		(setq boat-cap 6)


		(let ( (solution (mis-can-solve (list (list initial (heuristic initial goal) nil)) nil goal boat-cap)))
		  (cond
			((state-eq (caar solution) goal)
			 (format t "SOLUTION GENERATED. THE PATH TO THE GOAL IS DISPLAYED. ~%")
			 (dolist (x (rebuild-path solution (car solution))) (print x))
			 (format t " ~%")
			 )
			(t
			 (format t "THERE IS NO SOLUTION. THE FOLLOWING (STATE HEURISTIC PARENT) TRIPLES WERE GENERATED. ~%")
			 (dolist (x solution) (print x))))))
	)


(defun move (source dest mis-num can-num &optional (boat-num 1) new-source)
	"move the boat from source to dest with mis-num missionaries and can-num cannibals and boat-num boats"
  (cond
	((null source) (list new-source dest))

	((and (> mis-num 0) (member (car source) *missionaries*))
	 (move (cdr source) (cons (car source) dest) (- mis-num 1) can-num boat-num new-source))

	((and (> can-num 0) (member (car source) *cannibals*))
	 (move (cdr source) (cons (car source) dest) mis-num (- can-num 1) boat-num new-source))

	((and (> boat-num 0) (eq (car source) 'B))
	 (move (cdr source) (cons (car source) dest) mis-num can-num (- boat-num 1) new-source))

	(t (move (cdr source) dest mis-num can-num boat-num (cons (car source) new-source)))))

(defun mc-count (mc)
	"count the number of missionaries, cannibals and boats in the list mc"
  (cond
	((null mc) '(0 0 0))
	((member (car mc) *missionaries*) (mapcar '+ '(1 0 0) (mc-count (cdr mc))))
	((member (car mc) *cannibals*) (mapcar '+ '(0 1 0) (mc-count (cdr mc))))
	((eq (car mc) 'B) (mapcar '+ '(0 0 1) (mc-count (cdr mc))))
	(t (mapcar '+ (mc-count (cdr mc))))))


(defun count-atoms (l)
	"count the number of atoms in the list l"
  (cond
	((null l) 0)
	((atom (car l)) (+ 1 (count-atoms (cdr l))))
	(t (count-atoms (cdr l)))))

(defun valid-state-p (state)
	"check if the state is valid"
  (let ((ls-count (mc-count (car state)))
		(rs-count (mc-count (cadr state))))
	(cond
	  ((and (< (car ls-count) (cadr ls-count)) (> (car ls-count) 0)) nil)
	  ((and (< (car rs-count) (cadr rs-count)) (> (car rs-count) 0)) nil)
	  (t t))))


(defun gen-move-tuples (boat-cap mc-source-count)
	"generate all possible move tuples"
  (let ( (k (+ boat-cap 1)) (res nil) )
	(dotimes (i (+ boat-cap 1))
	  (dotimes (j k)
		(if (and (not (eq (+ i j) 0))
				 (<= i (car mc-source-count))
				 (<= j (cadr mc-source-count)))
		  (setq res (cons (list i j) res))))
	  (setq k (- k 1)))
	res))


(defun move-if-valid (source dest mis-num can-num &optional reverse-p)
	"move the boat from source to dest with mis-num missionaries and can-num cannibals"
  (let ((state (move source dest mis-num can-num)))
	(cond
	  ((null (valid-state-p state)) nil)
	  (reverse-p (list (reverse state)))
	  (t (list state)))))

(defun move-generator (state boat-cap)
	"generate all possible moves from the state"
  (cond
	((member 'B (car state))
	 (mapcan (lambda (tup) (move-if-valid (car state) (cadr state) (car tup) (cadr tup)))
			 (gen-move-tuples boat-cap (mc-count (car state)))))
	((member 'B (cadr state))
	 (mapcan (lambda (tup) (move-if-valid (cadr state) (car state) (car tup) (cadr tup) 1))
			 (gen-move-tuples boat-cap (mc-count (cadr state)))))))

(defun state-count (state)
	"count the number of missionaries, cannibals and boats in the state"
  (list (mc-count (car state)) (mc-count (cadr state))))


(defun state-eq (lhs rhs)
	"check if the states are equal"
  (cond ((equal (state-count lhs) (state-count rhs)) t)))

(defun heuristic (state goal)
	"calculate the heuristic value of the state"
  (heuristic-aux (state-count state) (state-count goal)))

(defun heuristic-aux (state goal)
	"calculate the heuristic value of the state"
  (cond
	((null (car state)) 0)
	(t (+ (min (caar state) (caar goal)) (min (car (cadr state)) (car (cadr goal)))
		  (heuristic-aux (list (cdr (car state)) (cdr (cadr state)))
					 (list (cdr (car goal)) (cdr (cadr goal))))))))

(defun goal-heuristic (goal)
	"calculate the heuristic value of the goal"
  (heuristic goal goal))

(defun gen-assoc (children parent goal)
	"generate a list of (child h parent) triples"
  (mapcar (lambda (child) (list child (heuristic child goal) parent)) children))

(defun member-assoc (m l)
	"check if m is a member of l"
  (cond
	((null l) nil)
	((state-eq  m (caar l)) (car l))
	(t (member-assoc m (cdr l)))))


(defun new-state-p (state open-assoc closed-assoc)
	"check if the state is new"
  (cond
	((or (member-assoc state open-assoc)
		  (member-assoc state closed-assoc)) nil)
	(t t)))

(defun insert-open-assoc (children-assoc open-assoc closed-assoc)
	"insert a list of (state h p) triples into open-assoc"
  (cond
	((null children-assoc) open-assoc)
	((new-state-p (caar children-assoc) open-assoc closed-assoc)
	 (insert-open-assoc (cdr children-assoc) (insert-state-assoc (car children-assoc) open-assoc) closed-assoc))
	(t (insert-open-assoc (cdr children-assoc) open-assoc closed-assoc))))

; Inserts a (state h p) triple (st-assoc) into st-assoc-list) sorted by heuristic value
(defun insert-state-assoc (st-assoc st-assoc-list)
	"insert a (state h p) triple into st-assoc-list sorted by heuristic value"
  (cond
	((null st-assoc-list) (list st-assoc))
	((>= (cadr st-assoc) (cadr (car st-assoc-list))) (cons st-assoc st-assoc-list))
	(t (cons (car st-assoc-list) (insert-state-assoc st-assoc (cdr st-assoc-list))))))

(defun mis-can-solve (open-assoc closed-assoc goal boat-cap)
	"generate the solution"
  (cond 
	((null open-assoc) closed-assoc)
	((eq (cadr (car open-assoc)) (goal-heuristic goal))
	 (append open-assoc closed-assoc))
	(t (mis-can-solve (insert-open-assoc
						(gen-assoc (move-generator (caar open-assoc) boat-cap) (caar open-assoc) goal)
						(cdr open-assoc) (cons (car open-assoc) closed-assoc))
					  (cons (car open-assoc) closed-assoc) goal boat-cap))))


(defun rebuild-path (solved-assoc goal-assoc-state)
	"rebuild the path to the goal"
  (let ((parent-assoc-state (member-assoc (caddr goal-assoc-state) solved-assoc)))
	(cond
	  ((null parent-assoc-state) (list (car goal-assoc-state)))
	  (t (append  (rebuild-path solved-assoc parent-assoc-state) (list (car goal-assoc-state)) ))))) 


(run)
