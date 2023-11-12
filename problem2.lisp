(defun misplaced-tiles (state goal-state)
"This function calculates the number of misplaced tiles in the current state compared to the goal state.
 This is used as the heuristic function."
  (let ((count 0))
    (dotimes (i 3)
      (dotimes (j 3)
        (when (not (= (nth i (nth j state)) (nth i (nth j goal-state))))
          (setq count (+ count 1))))
          )
    count))

(defun get-zero-pos (state)
  "Returns the position of the zero (blank) tile in the puzzle state."
  (let ((zero-pos nil))
    (dotimes (i 3 zero-pos)
      (dotimes (j 3)
        (when (= (elt (elt state i) j) 0)
          (setq zero-pos (list i j))
          (return zero-pos))))))

(defun swap-elements (list row1 col1 row2 col2)
  "Swaps elements between two positions (row1, col1) and (row2, col2) in a 2D list."
  (let* ((element1 (nth col1 (nth row1 list)))
         (element2 (nth col2 (nth row2 list))))
    (setf (nth col1 (nth row1 list)) element2)
    (setf (nth col2 (nth row2 list)) element1))
  list)

(defun copy-deep (orig)
  "Create a deep copy of a nested list."
  (if (listp orig)
      (mapcar #'copy-deep orig)
      orig))


(defun is-solved (state goal-state)
  "Checks if the given state is equal to the goal state."
  (equal state goal-state))

(defun generate-successors (state)
  "Generates successors by moving the zero (blank) tile in the puzzle state."
  (let* ((successors '())
         (moves '((0 . -1) (0 . 1) (-1 . 0) (1 . 0)))
         (zero-pos (get-zero-pos state)))
    (when zero-pos
      (dolist (move moves)
        (let* ((new-row  (+ (car zero-pos) (car move)))
               (new-col  (+ (cadr zero-pos) (cdr move))))
          (when (and (>= new-row 0) (< new-row 3)
                     (>= new-col 0) (< new-col 3))
            (let ((new-state (copy-deep state)))
              (swap-elements new-state (car zero-pos) (cadr zero-pos) new-row new-col)
              (push new-state successors)))))
    successors)))

(defun is-solvable (state)
  "Determines if a given puzzle state is solvable."
  (let* ((flattened-state (remove 0 (apply #'append state)))
         (inversion-count
          (loop for i from 0 to (- (length flattened-state) 2)
                sum (loop for j from (+ i 1) to (- (length flattened-state) 1)
                          count (when (> (nth i flattened-state) (nth j flattened-state)) t)))))
    (evenp inversion-count)))


(defun add-to-PM (parent-map successor-state)
  "Adds a new state to the parent map for path reconstruction"
  (let ((last (car (last parent-map))))
    (setf (car (last parent-map)) (append last (list successor-state)))
    parent-map))


(defun a-star-search-list (start-state goal-state)
  "Performs A* search on the given puzzle state."
  (let* ((open-list (list (list start-state 0 (misplaced-tiles start-state goal-state))))
         (closed-set '())
         (parent-map (list))
         (expanded-nodes 0))
    (loop
      (when (null open-list)
        (return nil))
      (let* ((current-state (caar open-list))
             (g (cadar open-list))
             (h (car (last (car open-list)))))
        (setf open-list (cdr open-list))
        (push current-state closed-set)
        (when (is-solved current-state goal-state)
          (format t "Expanded Nodes: ~a~% ~%" expanded-nodes)
          (return (reverse parent-map)))
        (let* ((successors (generate-successors current-state))
               (current-parent (list current-state)))
          (push current-parent parent-map)
          (setf parent-map (reverse parent-map))
          (dolist (successor-state successors)
            (unless (member successor-state closed-set :test #'equal)
              (let* ((g-successor (+ g 1))
                     (h-successor (misplaced-tiles successor-state goal-state))
                     (f-successor (+ g-successor h-successor)))
                (push (list successor-state g-successor h-successor) open-list)
                (setf open-list (sort open-list (lambda (x y) (< (caddr x) (caddr y)))))
                (setf parent-map (add-to-PM parent-map successor-state))
                (incf expanded-nodes)
                )))
            (setf parent-map (reverse parent-map))
          )
    ))))

(defun is-solvable (state)
  "Determines if a given puzzle state is solvable."
  (let* ((flattened-state (remove 0 (apply #'append state)))
         (inversion-count
          (loop for i from 0 to (- (length flattened-state) 2)
                sum (loop for j from (+ i 1) to (- (length flattened-state) 1)
                          count (when (> (nth i flattened-state) (nth j flattened-state)) t)))))
    (evenp inversion-count)))


(defun find-path (start goal-state)
  "Finds the path from the start state to the goal state."
  ;; (format t "goal state: ~a~%" goal-state)
  (let* ((solution (a-star-search-list start goal-state))
         (parent-map (reverse solution))
         (parent (first (first parent-map)))
         (path (list goal-state))
         )
    (setf parent-map (cdr parent-map))
    (loop
      (when (equal parent start)
      ;; (format t "path~a~%" path)
        (push start path)
        (return path))
      ;; (format t "~a~%" parent)
      (push parent path)
      (dolist (e parent-map)
        (when (member parent e :test #'equal)
          (setf parent (first e))
          (setf parent-map (remove (find e parent-map :test #'equal) parent-map :test #'equal))
          (return))))
    ))

(defun print-path (path)
  "Prints the path in the desired format."
  (dolist (state path)
    (dolist (row state)
      (format t "~{~a ~}~%" row))
    (format t "~%"))
  nil)

(defun find-path-and-print (start goal-state)
  "Finds the path from the start state to the goal state and prints it."
  (let* ((path (find-path start goal-state)))
  ;; (format t "path~a~%" path)
    (when path
      (print-path path))))


(defvar *start-state1* '((0 1 3) (4 2 5) (7 8 6))) ;;  ((E,1,3),(4,2,5),(7,8,6))
(defvar *goal-state1* '((1 2 3) (4 5 6) (7 8 0)))

(find-path-and-print *start-state1* *goal-state1*)


;; (format t "Test for Solvable Puzzle: ~%" )
;; (if (is-solvable *start-state1*)
;;     (find-path *start-state1* *goal-state1*)
;;     (format t "The puzzle is not solvable.~%"))

;; (defvar *start-state2* '((1 0 2) (5 4 3) (7 8 6))) ;; not solvable 
;; (defvar *goal-state2* '((1 2 3) (4 5 6) (7 8 0)))

;; (format t "Test for not Solvable Puzzle: ~%")
;; (if (is-solvable *start-state2*)
;;     (find-path *start-state2* *goal-state2*)
;;     (format t "The puzzle is not solvable."))