;; --- Water Jug Problem: State and Actions ---

(defconstant +cap4+ 4) ; 4-gallon jug capacity
(defconstant +cap3+ 3) ; 3-gallon jug capacity
(defconstant +goal+ 2) ; Target amount

;; State is a list: (G4 G3)

;; --- Action Functions (Move Rules) ---

;; R1: Fill the 4-gallon jug.
(defun fill4 (state)
  (let ((g4 (first state)) (g3 (second state)))
    (when (< g4 +cap4+)
      (list 'fill-4-gal (list +cap4+ g3)))))

;; R2: Fill the 3-gallon jug.
(defun fill3 (state)
  (let ((g4 (first state)) (g3 (second state)))
    (when (< g3 +cap3+)
      (list 'fill-3-gal (list g4 +cap3+)))))

;; R3: Empty the 4-gallon jug.
(defun empty4 (state)
  (let ((g4 (first state)) (g3 (second state)))
    (when (> g4 0)
      (list 'empty-4-gal (list 0 g3)))))

;; R4: Empty the 3-gallon jug.
(defun empty3 (state)
  (let ((g4 (first state)) (g3 (second state)))
    (when (> g3 0)
      (list 'empty-3-gal (list g4 0)))))

;; R5 & R7: Pour 4-gal to 3-gal
(defun pour4to3 (state)
  (let* ((g4 (first state)) (g3 (second state))
         (space3 (- +cap3+ g3))
         (transfer (min g4 space3))
         (new-g4 (- g4 transfer))
         (new-g3 (+ g3 transfer)))
    (when (> transfer 0)
      (list 'pour-4-to-3 (list new-g4 new-g3)))))

;; R6 & R8: Pour 3-gal to 4-gal
(defun pour3to4 (state)
  (let* ((g4 (first state)) (g3 (second state))
         (space4 (- +cap4+ g4))
         (transfer (min g3 space4))
         (new-g4 (+ g4 transfer))
         (new-g3 (- g3 transfer)))
    (when (> transfer 0)
      (list 'pour-3-to-4 (list new-g4 new-g3)))))

;; List of all action functions
(defvar *jug-actions* (list #'fill4 #'fill3 #'empty4 #'empty3 #'pour4to3 #'pour3to4))

;; Generates all valid successor states from the current state
(defun generate-jug-successors (state)
  (remove nil
          (loop for func in *jug-actions*
                collect (funcall func state))))

;; Checks if the goal is satisfied
(defun goal-satisfied (state)
  (or (= (first state) +goal+)
      (= (second state) +goal+)))

;; --- Planner (Breadth-First Search - BFS) ---

;; BFS finds the shortest path by exploring states level by level.
;; Nodes in the queue are lists: (State (Action1 Action2 ...))
(defun jug-plan-bfs (start-state)
  (let ((queue (list (list start-state nil))) ; Queue holds: (Current-State Plan-So-Far)
        (visited (list start-state)))       ; Visited holds just the state
    
    (loop 
      ;; Dequeue the first element, or stop if the queue is empty.
      for current-node = (pop queue)
      
      ;; Termination check
      unless current-node do (return nil)
      
      for current-state = (first current-node)
      for plan-so-far = (second current-node)
      
      do 
      ;; Check for Goal State
      (when (goal-satisfied current-state)
        (return (reverse plan-so-far)))
      
      ;; Generate Successors
      (loop for (action new-state) in (generate-jug-successors current-state)
            do
            (unless (member new-state visited :test 'equal)
              ;; Add to visited list
              (push new-state visited)
              ;; Enqueue the new node (New-State New-Plan)
              (setf queue (append queue (list (list new-state (cons action plan-so-far))))))))))

;; --- Example ---

(defvar *jug-init-state* '(0 0)) ; Use the star-naming convention for global variables

;; --- FIX APPLIED HERE: Using double quotes and correct format directives ---
(format t "~%--- Water Jug Plan (BFS) ---~%")
(format t "Initial State: ~A~%" *jug-init-state*)
(format t "Plan Found: ~A~%" (jug-plan-bfs *jug-init-state*))
