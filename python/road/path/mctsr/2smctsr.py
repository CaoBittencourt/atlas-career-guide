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

        # # normalize probs
        # vertices = vertices.with_columns(
        #     prob=pl.col.prob.cast(pl.Float64),
        #     prob=pl.col.prob / pl.col.prob.sum().over(partition_by=pl.col.career)
        # )

        # careers = careers.with_columns(
        #     prob=pl.col.prob.cast(pl.Float64),
        #     prob=pl.col.prob / pl.col.prob.sum().over(partition_by=pl.col.career)
        # )

        # starting vertex (not career)
        self.vertex = start

        # goal career (not vertex)
        self.goal = goal

        # current career (not vertex)
        _career = vertices.filter(
            pl.col.vertex == self.vertex,
        ).slice(0, 1)

        self.career = _career.select(pl.col.career).item()

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

        # # currently feasible vertex progs
        # self.vertexProgs = self.vertices.filter(
        #     pl.col.career != self.career,
        # )

        # career progression log
        # self.path = pl.DataFrame(
        #     {
        #         "year": 0,
        #         "career": self.career,
        #         "x": _career.select(pl.col.x).item(),
        #         "t": _career.select(pl.col.t).item(),
        #         "xCum": _career.select(pl.col.x).item(),
        #         "tCum": _career.select(pl.col.t).item(),
        #         "xReset": False,
        #         "tReset": False,
        #         "cost": 0,
        #     }
        # )

    def getPossibleActions(self):
        # remove current career
        # select next career
        # filter next vertices
        # select next vertex

        # first stage:
        # remove current career
        self.careers = self.careers.filter(pl.col.careerTo != self.career)
        self.vertices = self.vertices.filter(pl.col.career != self.career)

        self.careerProgs = self.careers.filter(pl.col.career == self.career)

        # randomly select a career
        self.career = np.random.choice(
            a=self.careerProgs.select(pl.col.careerTo).to_series(),
            size=1,
            p=self.careerProgs.select(pl.col.prob).to_series()
            / self.careerProgs.select(pl.col.prob).sum().item(),
        ).item()

        # second stage:
        # randomly select a vertex
        _vertexProgs = self.vertices.filter(pl.col.career == self.career)

        self.vertex = np.random.choice(
            a=_vertexProgs.select(pl.col.vertex).to_series(),
            size=1,
            p=_vertexProgs.select(pl.col.prob).to_series()
            / _vertexProgs.select(pl.col.prob).sum().item(),
        ).item()

        return self

    # def getPossibleActions(self):
    #     # first stage:
    #     # randomly select a career
    #     _careerTo = np.random.choice(
    #         a=self.careers.select(pl.col.careerTo).to_series(),
    #         size=1,
    #         p=self.careers.select(pl.col.prob).to_series()
    #         / self.careers.select(pl.col.prob).sum().item(),
    #     ).item()

    #     # second stage:
    #     # randomly select a vertex
    #     _vertexProgs = self.vertexProgs.filter(
    #         pl.col.career == _careerTo,
    #     )

    #     _vertexTo = np.random.choice(
    #         a=_vertexProgs.select(pl.col.vertex).to_series(),
    #         size=1,
    #         p=_vertexProgs.select(pl.col.prob).to_series()
    #         / _vertexProgs.select(pl.col.prob).sum().item(),
    #     ).item()

    #     return {
    #         "careerTo": _careerTo,
    #         "vertexTo": _vertexTo,
    #     }

    def takeAction(self):
        # update timeline
        # _vertex = self.vertices.filter(pl.col.vertex == vertexTo)
        # pl.concat(
        #     [
        #         self.path,
        #         pl.DataFrame(
        #             {
        #                 "year": 0,
        #                 "career": self.career,
        #                 "x": _vertex.select(pl.col.x).item(),
        #                 "t": _vertex.select(pl.col.t).item(),
        #                 "xCum": self.path.select(pl.col.xCum).tail(1).item()
        #                 + _vertex.select(pl.col.x).item(),
        #                 "tCum": self.path.select(pl.col.tCum).tail(1).item()
        #                 + _vertex.select(pl.col.t).item(),
        #                 "xReset": False,
        #                 "tReset": False,
        #                 "cost": 1,
        #             }
        #         ),
        #     ]
        # )

        # self.careerProgs = self.careers.filter(pl.col.career == self.career)

        # self.careers = self.careers.filter(pl.col.career != self.career)
        # self.vertices = self.vertices.filter(pl.col.career != self.career)

        return self

    def isTerminal(self):
        # game ends when the target career is reached
        return self.career == self.goal

    def getReward(self):
        # the smaller the cost, the higher the reward
        return -self.path.select(pl.col.cost).sum().item()


# endregion
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

while not pathfinder.isTerminal():
    print(f"current vertex: {pathfinder.vertex}")
    pathfinder = pathfinder.getPossibleActions()
    print(f"current vertex: {pathfinder.vertex}")
    pathfinder = pathfinder.takeAction()
    # print(f"current cost: {pathfinder.path}")

# endregion
