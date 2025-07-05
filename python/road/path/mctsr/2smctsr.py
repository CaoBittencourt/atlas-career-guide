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
        _start = vertices.filter(
            pl.col.vertex == self.vertex,
        ).slice(0, 1)

        self.career = _start.select(pl.col.career).item()

        # all (feasible) careers progs
        self.careers = careers.filter(pl.col.careerTo != self.career)

        # all (feasible) vertex progs
        self.vertices = vertices.filter(pl.col.career != self.career)

        # currently feasible career progs
        self.progs = self.careers.filter(pl.col.career == self.career)

        # career progression log
        self.path = pl.DataFrame(
            {
                "career": self.career,
                "x": _start.select(pl.col.x).item(),
                "t": _start.select(pl.col.t).item(),
                "xReset": False,
                "tReset": False,
                "cost": 0,
            }
        )

    def getPossibleActions(self):
        # remove current career
        # select next career
        # filter next vertices
        # select next vertex

        # first stage:
        # remove current career
        self.careers = self.careers.filter(pl.col.careerTo != self.career)
        self.vertices = self.vertices.filter(pl.col.career != self.career)
        self.progs = self.careers.filter(pl.col.career == self.career)

        # randomly select a career
        _career = np.random.choice(
            a=self.progs.select(pl.col.careerTo).to_series(),
            size=1,
            p=self.progs.select(pl.col.prob).to_series()
            / self.progs.select(pl.col.prob).sum().item(),
        ).item()

        # second stage:
        # randomly select a vertex
        _vertexProgs = self.vertices.filter(pl.col.career == _career)

        _vertex = np.random.choice(
            a=_vertexProgs.select(pl.col.vertex).to_series(),
            size=1,
            p=_vertexProgs.select(pl.col.prob).to_series()
            / _vertexProgs.select(pl.col.prob).sum().item(),
        ).item()

        return {
            "careerTo": _career,
            "vertexTo": _vertex,
        }

    def _cost(ßkq, xk, xq, tk, tq):
        # equivalent similarity
        ßkqEq = ßkq * (ßkq >= 0.5)

        # experience gap
        xReq = np.maximum(xq - xk * ßkqEq, 0) / ßkq

        # education gap
        tReq = np.maximum(tq - tk * ßkqEq, 0) / ßkq

        xReq = np.minimum(xq, xReq)
        tReq = np.minimum(tq, tReq)
        # assume one must have all equivalent years
        # before attempting to switch careers
        # assume order of study and work doesn't matter
        # assume reset cost is zero
        # movement types:
        # - restart x, recycle t
        # - restart t, recycle x
        # - restart t, restart x
        # - recycle t, recycle x

        return {
            "work": xReq,
            "study": tReq,
            "total": xReq + tReq,
            "xReset": xReq == xq,
            "tReset": tReq == tq,
        }

    def cost(self, careerTo: int, vertexTo: int):
        _careers = self.careers.filter(pl.col.career == self.career).filter(
            pl.col.careerTo == careerTo
        )

        _vertex = self.vertices.filter(pl.col.vertex == self.vertex)
        _vertexTo = self.vertices.filter(pl.col.vertex == vertexTo)

        return self._cost(
            xk=_vertex.select(pl.col.x).item(),
            xq=_vertexTo.select(pl.col.x).item(),
            tk=_vertex.select(pl.col.t).item(),
            tq=_vertexTo.select(pl.col.t).item(),
        )

    def takeAction(self, careerTo: int, vertexTo: int):
        # update timeline
        _vertex = self.vertices.filter(pl.col.vertex == vertexTo)
        _cost = self.cost(careerTo, vertexTo)

        self.path = pl.concat(
            [
                self.path,
                pl.DataFrame(
                    {
                        "career": self.career,
                        "x": _vertex.select(pl.col.x).item(),
                        "t": _vertex.select(pl.col.t).item(),
                        "xReset": _cost["xReset"],
                        "tReset": _cost["tReset"],
                        "cost": _cost["total"],
                    }
                ),
            ]
        )

        # update state
        self.career = careerTo
        self.vertex = vertexTo

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
    pathfinder = pathfinder.takeAction(**pathfinder.getPossibleActions())
    # print(f"current cost: {pathfinder.path}")

# endregion
