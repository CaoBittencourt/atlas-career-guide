# setup
# region: imports
from mcts import mcts
import polars as pl
import numpy as np
import os
from numbers import Number

# endregion
# region: data
careers = pl.read_parquet(
    os.path.join(
        os.getenv("ATLAS_DATA"),
        "road",
        "careers.parquet",
    )
)

vertices = pl.read_parquet(
    os.path.join(
        os.getenv("ATLAS_DATA"),
        "road",
        "vertices.parquet",
    )
)

# endregion
# model
# region: requirements
# In order to run MCTS, you must implement a State class which can fully describe the state of the world. It must also implement four methods:

# getPossibleActions(): Returns an iterable of all actions which can be taken from this state
# takeAction(action): Returns the state which results from taking action action
# isTerminal(): Returns whether this state is a terminal state
# getReward(): Returns the reward for this state. Only needed for terminal states.
# You must also choose a hashable representation for an action as used in getPossibleActions and takeAction. Typically this would be a class with a custom __hash__ method, but it could also simply be a tuple or a string.


# endregion
# region: two-stage monte carlo tree search roadmap
class Pathfinder:
    def __init__(
        self,
        start: int,  # start vertex
        goal: int,  # target career
        careers: pl.DataFrame,  # career probabilities (stage 1)
        vertices: pl.DataFrame,  # vertex probabilities and costs (stage 2)
    ):
        assert all(
            np.isin(
                [
                    "career",
                    "vertex",
                    "x",
                    "t",
                    "prob",
                ],
                vertices.columns,
            )
        )

        assert all(
            np.isin(
                [
                    "career",
                    "careerTo",
                    "similarity",
                    "prob",
                ],
                careers.columns,
            )
        )

        assert np.isin(start, vertices.select(pl.col.vertex).unique().to_numpy())
        assert np.isin(goal, careers.select(pl.col.careerTo).unique().to_numpy())

        # starting vertex (not career)
        self.vertex = start

        # goal career (not vertex)
        self.goal = goal

        # current career (not vertex)
        self.career = (
            vertices.filter(
                pl.col.vertex == self.vertex,
            )
            .slice(0, 1)
            .select(pl.col.career)
            .item()
        )

        # all (feasible) careers progs
        self.careers = careers.filter(
            pl.col.careerTo != self.career,
        )

        # all (feasible) vertex progs
        self.vertices = vertices.filter(
            pl.col.career != self.career,
        )

        # currently feasible career progs
        self.careerProgs = self.careers.filter(
            pl.col.career == self.career,
        )

        # currently feasible vertex progs
        self.vertexProgs = self.vertices.filter(
            pl.col.career != self.career,
        )

        # career progression log
        self.path = pl.DataFrame(
            # int career id
            # num timeline
            # num years of experience
            # num years of education
            # num cumulative years of experience
            # num cumulative years of education
            # bool xReset
            # bool tReset
        )

    def getPossibleActions(self):
        # first stage:
        # randomly select a career
        _careerTo = np.random.choice(
            a=self.careers.select(pl.col.careerTo).to_series(),
            size=1,
            p=self.careers.select(pl.col.prob).to_series(),
        ).item()

        # second stage:
        # randomly select a vertex
        _vertexProgs = self.vertexProgs.filter(
            pl.col.career == _careerTo,
        )

        _vertexTo = np.random.choice(
            a=_vertexProgs.select(pl.col.vertex).to_series(),
            size=1,
            p=_vertexProgs.select(pl.col.prob).to_series(),
        ).item()

        return {
            "careerTo": _careerTo,
            "vertexTo": _vertexTo,
        }

    def takeAction(self, careerTo: int, vertexTo: int):
        # # update timeline
        # self.path = pl.concat(
        #     [
        #         self.path,
        #         self.vertexProgs.filter(
        #             pl.col.vertex == self.vertex,
        #             pl.col.vertexTo == vertexTo,
        #         ).select(
        #             pl.col.career
        #         ),
        #     ]
        # )

        # update state
        self.career = careerTo
        self.vertex = vertexTo

        self.careers = self.careers.filter(pl.col.careerTo != self.career)
        self.careerProgs = self.careers.filter(pl.col.career == self.career)

        self.vertices = self.vertices.filter(pl.col.careerTo != self.career)
        self.vertexProgs = self.vertices.filter(pl.col.vertex == self.vertex)

        return self

    def isTerminal(self):
        # game ends when the target career is reached
        return self.career == self.goal

    def getReward(self):
        # the smaller the cost, the higher the reward
        return -self.path.select(pl.col.cost).sum().item()


# endregion
# # region: state class
# class Pathfinder:
#     def __init__(
#         self,
#         start: int,
#         goal: int,
#         graph: pl.DataFrame,
#     ):
#         assert all(
#             np.isin(
#                 [
#                     "vertex",
#                     "vertex.to",
#                     "prob",
#                     "cost",
#                     "inverse.payoff",
#                 ],
#                 graph.columns,
#             )
#         )

#         self.graph = graph.select(
#             "vertex",
#             "vertex.to",
#             "prob",
#             "cost",
#         )
#         # normalize probs later

#         self.vertex = start
#         self.goal = goal
#         self.cost = 0

#         self.paths = self.graph.filter(
#             pl.col("vertex") == self.vertex,
#         ).select(
#             pl.col("vertex.to"),
#             pl.col("prob"),
#             pl.col("cost"),
#         )

#     def getPossibleActions(self):
#         # randomly select a vertex
#         return np.random.choice(
#             a=self.paths["vertex.to"],
#             size=1,
#             p=self.paths["prob"] / self.paths["prob"].sum(),
#         ).item()

#     def takeAction(self, vertexTo: int):
#         # update total cost
#         self.cost += (
#             self.paths.filter(
#                 pl.col("vertex.to") == vertexTo,
#             )
#             .select(pl.col("cost"))
#             .item()
#         )

#         # update graph data frame
#         self.graph = self.graph.filter(
#             pl.col("vertex") != self.vertex,
#             pl.col("vertex.to") != self.vertex,
#             pl.col("vertex.to") != vertexTo,
#         )

#         # update paths data frame
#         self.paths = self.graph.filter(
#             pl.col("vertex") == vertexTo,
#         )

#         # update current vertex
#         self.vertex = vertexTo

#         return self

#     def isTerminal(self):
#         # game ends when the target vertex is reached
#         return self.vertex == self.goal

#     def getReward(self):
#         # the smaller the cost, the higher the reward
#         return -self.cost


# # endregion
# example
# region: all careers
k = 1
q = 2
pathfinder = Pathfinder(
    start=np.random.choice(
        a=vertices.filter(pl.col.career == k).select(pl.col.vertex).to_series(),
        size=1,
        p=vertices.filter(pl.col.career == k).select(pl.col.prob).to_series(),
    ).item(),
    goal=q,
    careers=careers,
    vertices=vertices,
)

pathfinder.career
pathfinder.goal
pathfinder.getPossibleActions()
# (
#     pathfinder.vertexProgs.group_by(pl.col.career)
#     .agg(prob=pl.col.prob.sum())
#     .with_columns(prob_1=pl.col.prob.round(4) == 1)
#     .select(pl.col.prob_1)
#     .to_series()
#     .all()
# )

pathfinder.getPossibleActions()

while not pathfinder.isTerminal():
    print(f"current vertex: {pathfinder.vertex}")
    print(f"current cost: {pathfinder.cost}")
    pathfinder = pathfinder.takeAction(pathfinder.getPossibleActions())

# endregion
