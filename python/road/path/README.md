# `mctsr`: Monte Carlo Tree Search Roadmap

The `mctsr` module aims to apply Monte Carlo Tree Search (MCTS), a probabilistic pathfinding algorithm, to solve the career roadmap problem. In order to do so, we must simplify the career grid used for the Dijkstra implementation, as MCTS's feasibility requires the program to iterate quickly over the grid's nodes. This, however, cannot be acchieved on a low-probability, highly specific grid, as the one developed for Dijkstra's algorithm.

# Simplified Grid
To simplify the career path grid,