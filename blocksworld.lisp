;; --- Blocks World: State, Actions, and Planner ---

;; Define the actions (Preconditions, Delete-List, Add-List)
;; Each action is a list: (Action-Name Arg1 Arg2 :pre P-List :del D-List :add A-List)
;; NIL is used as a placeholder for the second block (Y) in single-argument actions.
(defun blocks-actions (block-a block-b)
  (list
    ;; (pickup X)
    (list 'pickup block-a nil
          :pre (list (list 'clear block-a) (list 'ontable block-a) (list 'holding 'none))
          :del (list (list 'ontable block-a) (list 'clear block-a) (list 'holding 'none))
          :add (list (list 'holding block-a)))
    
    ;; (putdown X)
    (list 'putdown block-a nil
          :pre (list (list 'holding block-a))
          :del (list (list 'holding block-a))
          :add (list (list 'ontable block-a) (list 'clear block-a) (list 'holding 'none)))
    
    ;; (unstack X Y)
    (list 'unstack block-a block-b
          :pre (list (list 'on block-a block-b) (list 'clear block-a) (list 'holding 'none))
          :del (list (list 'on block-a block-b) (list 'clear block-a) (list 'holding 'none))
          :add (list (list 'holding block-a) (list 'clear block-b)))
    
    ;; (stack X Y)
    (list 'stack block-a block-b
          :pre (list (list 'holding block-a) (list 'clear block-b))
          :del (list (list 'holding block-a) (list 'clear block-b))
          :add (list (list 'on block-a block-b) (list 'clear block-a) (list 'holding 'none)))))

;; --- Utility Functions ---

;; Checks if all facts in a list (L1) are present in the state (L2)
(defun all-members (l1 l2)
  (every (lambda (x) (member x l2 :test 'equal)) l1))

;; Applies the action's effects to generate a new state
(defun apply-effects (state del-list add-list)
  (let ((new-state (set-difference state del-list :test 'equal)))
    (union new-state add-list :test 'equal)))

;; Generates successors
(defun generate-successors (state all-blocks)
  (loop for action-list in 
        (loop for a in all-blocks
              append (loop for b in all-blocks
                           unless (equal a b)
                           append (blocks-actions a b))
              append (blocks-actions a nil)) 
        
        ;; Extract action components
        for name = (first action-list)
        for arg1 = (second action-list)
        for arg2 = (third action-list)
        
        ;; Extract properties starting after the arguments (index 3)
        for props = (cdddr action-list)
        for pre = (getf props :pre)
        for del = (getf props :del)
        for add = (getf props :add)

        ;; Construct the final action name list (without NIL placeholders)
        for action-name-with-args = (remove nil (list name arg1 arg2))
        
        when (all-members pre state)
        collect (list action-name-with-args
                      (apply-effects state del add))))

;; --- Planner (Breadth-First Search - BFS) ---

(defun blocks-plan-bfs (start-state goal-state all-blocks)
  (let ((queue (list (list start-state nil))) ; Queue holds: (Current-State Plan-So-Far)
        (visited (list start-state)))       ; Visited holds just the state for fast checking
    
    (loop 
      ;; --- ALL ITERATION CLAUSES GROUPED HERE ---
      for current-node = (pop queue)
      
      ;; 1. Check for termination immediately after popping
      unless current-node do (return nil)
      
      ;; 2. Assign variables for the rest of the loop body
      for current-state = (first current-node)
      for plan-so-far = (second current-node)
      
      do 
      ;; Check for Goal State
      (when (all-members goal-state current-state)
        (return (reverse plan-so-far)))
      
      ;; Generate Successors
      (loop for (action new-state) in (generate-successors current-state all-blocks)
            do
            (unless (member new-state visited :test 'equal)
              ;; Add to visited list
              (push new-state visited)
              ;; Enqueue the new node (New-State New-Plan)
              (setf queue (append queue (list (list new-state (cons action plan-so-far))))))))))

;; --- Example ---

(defvar *blocks* '(a b c)) ; Use standard Lisp naming convention (with *)

;; State: All blocks on the table.
(defvar *init-state* '((ontable a) (ontable b) (ontable c) (clear a) (clear b) (clear c) (holding none)))

;; Goal: A on B, B on C.
(defvar *goal-state* '((on a b) (on b c)))

;; --- FIX APPLIED HERE: Using correct double quotes and format directives (e.g., ~A) ---
(format t "~%--- Blocks World Plan (BFS) ---~%")
(format t "Initial State: ~A~%" *init-state*)
(format t "Goal State: ~A~%" *goal-state*)
(format t "Plan Found: ~A~%" (blocks-plan-bfs *init-state* *goal-state* *blocks*))
