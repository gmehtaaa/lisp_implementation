# Common Lisp Blocks World Planner (BFS)

## Problem Overview
This project implements the classic **Blocks World** problem using **Common Lisp**, demonstrating a solution via **Breadth-First Search (BFS)** to guarantee the **shortest, optimal plan**.

---

## Problem Statement
The **Blocks World** is a foundational AI planning task used to study **action sequence synthesis**.

| Aspect | Detail |
|---------|---------|
| **Problem** | Find the optimal (shortest) sequence of actions (`pickup`, `stack`, etc.) to move blocks from an initial configuration to a desired goal state (`A on B, B on C`). |
| **State Representation** | A state is represented as a list of facts, e.g. `((ONTABLE A) (CLEAR B) (HOLDING NONE))`. |

---

## Algorithm and Implementation Details
The solver uses **Breadth-First Search (BFS)** to ensure the shortest possible solution.

### Core Lisp Structures

| Lisp Structure | Function / Variable | Description |
|----------------|----------------------|--------------|
| **Search Engine** | `blocks-plan-bfs` | Implements BFS using a queue to explore states layer by layer, guaranteeing optimality. |
| **State Queue** | `queue` (list) | Stores nodes as `(Current-State Plan-So-Far)` for FIFO processing. |
| **Actions** | `blocks-actions` | Defines the four core operators (`pickup`, `stack`, etc.) using Lisp property lists (`:pre`, `:del`, `:add`) for effects. |
| **State Management** | `apply-effects` | Uses `set-difference` and `union` for efficient, non-destructive state updates. |

---

## Applications and Sample Execution

### Real-World Lisp Use Cases
The symbolic planning and state-space management principles demonstrated here are foundational to many Lisp-based systems:

| Domain | Description |
|---------|-------------|
| **Expert Systems** | Early AI systems like *Dendral* and *Mycin* used Lisp for symbolic reasoning and rule-based inference. |
| **Automated Planning & Robotics** | Used for planning high-level control sequences in autonomous robots and drones. |
| **Symbolic Mathematics** | Systems like *Macsyma* and *Maxima* perform symbolic differentiation, integration, and equation solving. |
| **CAD/CAM** | Lisp dialects are used to automate design tasks in computer-aided design and manufacturing software. |

---

### Sample Execution

To run the solver:

1. Copy the Lisp code into a Common Lisp REPL or environment.  
2. Run the planner (final lines in the file usually trigger the execution).
   
   Plan Found: ((PICKUP B) (STACK B C) (PICKUP A) (STACK A B))

-----------
-----------
-----------

# Common Lisp Water Jug Problem Solver (BFS)

## Problem Overview
This project solves the **Water Jug Problem** (4 & 3 gallons, target 2 gallons) using **Common Lisp** and the **Breadth-First Search (BFS)** algorithm to find the most efficient plan.

---

## Problem Statement
The **Water Jug Problem** is a classic state-space search problem emphasizing **constraint satisfaction** and **arithmetic reasoning**.

| Aspect | Detail |
|---------|---------|
| **Problem** | Find the shortest sequence of fills, empties, and pours to obtain exactly 2 gallons in either the 4-gallon jug or the 3-gallon jug. |
| **State Representation** | A state is represented as a list of jug volumes: `(G4 G3)`. |

---

## Algorithm and Implementation Details
The program defines each operation as a function and employs **Breadth-First Search (BFS)** for guaranteed optimal plan discovery.

### Core Lisp Structures

| Lisp Structure | Function / Constant | Description |
|----------------|----------------------|--------------|
| **Search Engine** | `jug-plan-bfs` | Implements BFS for shortest plan computation. |
| **Action Functions** | `fill4`, `pour3to4`, etc. | Each function checks preconditions and performs arithmetic updates using constructs like `let*` and `min`. |
| **Queue / Visited Lists** | `queue`, `visited` | The queue tracks unexplored states (FIFO order); `visited` prevents revisiting previously explored states. |

---

## Applications and Sample Execution

### Real-World Lisp Use Cases
The constraint-solving and search logic demonstrated here form the basis for many real-world Lisp applications:

| Domain | Description |
|---------|-------------|
| **Financial Modeling & Trading** | Lisp is used for algorithmic trading and risk analysis due to its symbolic computation and rule-based modeling capabilities. |
| **Compiler Technology** | Lisp’s *code-as-data* paradigm supports macro-based compiler construction and metaprogramming. |
| **Constraint Logic Programming (CLP)** | Similar principles are used in airline crew scheduling, resource allocation, and timetabling problems. |
| **Autonomous Systems** | The planner’s logic models real-world decision-making processes used in autonomous vehicles and robotics. |

---

### Sample Execution

To run the solver:

1. Copy the Lisp code into a Common Lisp REPL or development environment.  
2. Evaluate the program (final lines execute the planner automatically).

#### Example Output
Plan Found: (FILL-3-GAL POUR-3-TO-4 FILL-3-GAL POUR-3-TO-4)

