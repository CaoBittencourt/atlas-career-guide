# setup
# region: imports
from mcts import mcts
import polars as pl
import numpy as np
import os
from pyreadr import read_r
from numbers import Number

# endregion
# region: data
graph = pl.read_csv("/home/Cao/storage/github/atlas/database/data/graph.csv")

# endregion
# model
# region: required functions
# In order to run MCTS, you must implement a State class which can fully describe the state of the world. It must also implement four methods:

# getPossibleActions(): Returns an iterable of all actions which can be taken from this state
# takeAction(action): Returns the state which results from taking action action
# isTerminal(): Returns whether this state is a terminal state
# getReward(): Returns the reward for this state. Only needed for terminal states.
# You must also choose a hashable representation for an action as used in getPossibleActions and takeAction. Typically this would be a class with a custom __hash__ method, but it could also simply be a tuple or a string.


# endregion
# region: state class
class Pathfinder:
    def __init__(
        self,
        start: int,
        goal: int,
        graph: pl.DataFrame,
    ):
        assert all(
            np.isin(
                [
                    "vertex",
                    "vertex.to",
                    "prob",
                    "cost",
                    "inverse.payoff",
                ],
                graph.columns,
            )
        )

        self.graph = graph.select(
            "vertex",
            "vertex.to",
            "prob",
            "cost",
        )
        # normalize probs later

        self.vertex = start
        self.goal = goal
        self.cost = 0

        self.paths = self.graph.filter(
            pl.col("vertex") == self.vertex,
        ).select(
            pl.col("vertex.to"),
            pl.col("prob"),
            pl.col("cost"),
        )

    def getPossibleActions(self):
        # randomly select a vertex
        return np.random.choice(
            a=self.paths["vertex.to"],
            size=1,
            p=self.paths["prob"] / self.paths["prob"].sum(),
        ).item()

    def takeAction(self, vertexTo: int):
        # update total cost
        self.cost += (
            self.paths.filter(
                pl.col("vertex.to") == vertexTo,
            )
            .select(pl.col("cost"))
            .item()
        )

        # update graph data frame
        self.graph = self.graph.filter(
            pl.col("vertex") != self.vertex,
            pl.col("vertex.to") != self.vertex,
            pl.col("vertex.to") != vertexTo,
        )

        # update paths data frame
        self.paths = self.graph.filter(
            pl.col("vertex") == vertexTo,
        )

        # update current vertex
        self.vertex = vertexTo

        return self

    def isTerminal(self):
        # game ends when the target vertex is reached
        return self.vertex == self.goal

    def getReward(self):
        # the smaller the cost, the higher the reward
        return -self.cost


# endregion
# example
# region: all occupations
pathfinder = Pathfinder(
    start=graph["vertex"].sample(1).item(),
    goal=graph["vertex"].sample(1).item(),
    graph=graph,
)

pathfinder.vertex
pathfinder.goal
pathfinder.cost
pathfinder.paths
pathfinder.graph
pathfinder.isTerminal()
pathfinder.getReward()

while not pathfinder.isTerminal():
    print(f"current vertex: {pathfinder.vertex}")
    print(f"current cost: {pathfinder.cost}")
    pathfinder = pathfinder.takeAction(pathfinder.getPossibleActions())

# endregion
