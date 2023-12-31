# Missionaries-and-Cannibals-and-the-8-Puzzle-in-Common-Lisp

## Introduction

This repository showcases the implementation of two classic problems in artificial intelligence, solved using Common Lisp. The first problem tackled is the "Missionaries and Cannibals" problem, where a group of missionaries and cannibals must navigate a river, ensuring certain constraints. The second problem addressed is the "8-Puzzle," a sliding puzzle game involving rearranging numbered tiles to reach a desired goal state.

## Problem 1: Missionaries and Cannibals

### Problem Description

The Missionaries and Cannibals problem involves moving missionaries and cannibals across a river while adhering to specific constraints. The objective is to move all the missionaries and cannibals from one side to the other without violating certain rules.

### Approach to Solving

- **Representation of State:** The state of the problem is represented as a list, with each element representing a person or the boat on a particular side of the river.
- **Valid State Check:** A function is implemented to check if a state is valid, ensuring that the number of cannibals does not outnumber the number of missionaries on either side of the river.
- **Generating Possible Moves:** All possible moves are generated by moving people from one side of the river to the other, respecting the boat's capacity.
- **Heuristic Function:** A heuristic function estimates the distance from the current state to the goal state, considering the number of people on each side and their distance from the goal.
- **Solution Path:** The solution path is reconstructed once the goal state is reached, displaying each step of the journey.

### Code Snippets

- `move`: Moves people from one side to another while considering the boat's capacity.
- `valid-state-p`: Checks if a state is valid according to the problem's constraints.
- `heuristic`: Calculates the heuristic value of a state.
- `mis-can-solve`: Implements the A* algorithm to find the solution.
- `rebuild-path`: Reconstructs the path from the initial state to the goal state.

### Testing and Validation

The program has been tested for scenarios with different numbers of cannibals and missionaries, and it successfully found solutions for each scenario.

## Problem 2: 8-Puzzle

### Problem Description

The 8-Puzzle problem involves transforming an initial state of a puzzle into a goal state by moving tiles on a grid while adhering to specific moves and constraints.

### Approach to Solving

- **Misplaced Tiles Heuristic:** A heuristic function calculates the number of tiles in the current state that are not in their correct positions compared to the goal state.
- **Search Algorithm:** The A* search algorithm is applied to explore the state space efficiently, selecting states based on their estimated cost.
- **Generating Successors:** Possible successor states are generated by moving the blank tile in the puzzle grid in different directions, evaluated based on their heuristic values.
- **Solvable Puzzle Check:** A check is included to determine if a given puzzle is solvable based on the inversion count.
- **Path Reconstruction:** Once the goal state is reached, the path from the initial state to the goal state is reconstructed by backtracking through the parent states.

### Code Snippets

- `misplaced-tiles`: Calculates the number of misplaced tiles in the current state compared to the goal state, serving as the heuristic function.
- `generate-successors`: Generates successor states by moving the blank tile in the puzzle.
- `a-star-search-list`: Performs A* search on the puzzle states.
- `is-solvable`: Determines if a given puzzle is solvable based on the inversion count.
- `find-path`: Finds the path from the initial state to the goal state and prints the steps.

### Testing and Validation

The program has been rigorously tested for both solvable and unsolvable puzzle scenarios to ensure correctness and robustness.

## How to Run

To run the program:

1. Go to [https://www.sbcl.org/platform-table.html](https://www.sbcl.org/platform-table.html)
2. Download the file by clicking on the green square (choose depending on your computer).
3. Complete the installation.
4. Open your terminal.
5. Run `sbcl` (if it's not running, try restarting your terminal or computer).
6. When `sbcl` is running, go to the files folder and open the terminal.
7. Run `sbcl --script problem1.lisp` or `sbcl --script problem2.lisp`.

## Conclusion

By addressing two classical problems, the Missionaries and Cannibals and the 8-Puzzle, this project demonstrates the versatility and power of Common Lisp in solving challenges in artificial intelligence. The provided programs offer optimal solutions and thoroughly tested implementations for both problems, making them valuable tools for problem-solving and education in the field of AI.
