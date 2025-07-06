# setup
# region: imports
from mcts import mcts
import polars as pl
import numpy as np
import os
from types import NoneType
from numbers import Number
from python.data import ist

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
# region: movement cost
def _cost(ßkq: float, xk: float, xq: float, tk: float, tq: float):
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
        "xReset": xReq > xq and xq != 0,
        "tReset": tReq > tq and tq != 0,
    }


# endregion
# region: two-stage monte carlo tree search roadmap
class Pathfinder:
    def __init__(
        self,
        start: int,  # start vertex
        goal: int,  # target career
        careers: pl.DataFrame,  # career probabilities (stage 1)
        vertices: pl.DataFrame,  # vertex probabilities and costs (stage 2)
        yearsMax: [float | int | NoneType] = None,
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

        # all (helpful) careers progs

        # # all careers progs
        self.careers = careers

        # all vertex progs
        self.vertices = vertices

        # time limit
        self.yearsMax = (
            self.vertices.filter(pl.col.career == self.goal)
            .select(pl.col.x, pl.col.t)
            .max()
            .sum_horizontal()
            .item()
            if ist.null(yearsMax)
            else yearsMax
        )

        # career progression log
        self.path = pl.DataFrame(
            {
                "career": self.career,
                "x": _start.select(pl.col.x).item(),
                "t": _start.select(pl.col.t).item(),
                "xReset": False,
                "tReset": False,
                "cost": 0.0,
            }
        )

    def getPossibleActions(self):
        # first stage:
        # feasible career progressions
        _careerProgs = self.careers.filter(pl.col.career == self.career)

        # randomly select a career
        _careerTo = np.random.choice(
            a=_careerProgs.select(pl.col.careerTo).to_series(),
            size=1,
            p=_careerProgs.select(pl.col.prob).to_series()
            / _careerProgs.select(pl.col.prob).sum().item(),
        ).item()

        # second stage:
        # feasible vertex progressions
        _vertexProgs = self.vertices.filter(pl.col.career == _careerTo)

        # randomly select a vertex
        _vertexTo = np.random.choice(
            a=_vertexProgs.select(pl.col.vertex).to_series(),
            size=1,
            p=_vertexProgs.select(pl.col.prob).to_series()
            / _vertexProgs.select(pl.col.prob).sum().item(),
        ).item()

        return {
            "careerTo": _careerTo,
            "vertexTo": _vertexTo,
        }

    def cost(self, careerTo: int, vertexTo: int):
        _careers = self.careers.filter(pl.col.career == self.career).filter(
            pl.col.careerTo == careerTo
        )

        _vertex = self.vertices.filter(pl.col.vertex == self.vertex)
        _vertexTo = self.vertices.filter(pl.col.vertex == vertexTo)

        return _cost(
            ßkq=_careers["similarity"].item(),
            xk=_vertex.select(pl.col.x).item(),
            xq=_vertexTo.select(pl.col.x).item(),
            tk=_vertex.select(pl.col.t).item(),
            tq=_vertexTo.select(pl.col.t).item(),
        )

    def takeAction(self, careerTo: int, vertexTo: int):
        # update timeline
        _vertexTo = self.vertices.filter(pl.col.vertex == vertexTo)
        _moveCost = self.cost(careerTo, vertexTo)

        self.path = pl.concat(
            [
                self.path,
                pl.DataFrame(
                    {
                        "career": careerTo,
                        "x": _vertexTo.select(pl.col.x).item(),
                        "t": _vertexTo.select(pl.col.t).item(),
                        "xReset": _moveCost["xReset"],
                        "tReset": _moveCost["tReset"],
                        "cost": _moveCost["total"],
                    }
                ),
            ]
        )

        # update state
        self.careers = (
            self.careers.filter(pl.col.career != self.career)
            .filter(pl.col.careerTo != self.career)
            .filter(pl.col.careerTo != careerTo)
        )

        # self.vertices = self.vertices.filter(pl.col.career != self.career).filter(
        #     pl.col.career != careerTo
        # )

        self.career = careerTo
        self.vertex = vertexTo

        return self

    def isTerminal(self):
        # game ends when the target career is reached
        return (
            self.career == self.goal
            or self.yearsMax <= self.path.select(pl.col.cost).sum().item()
        )

    def getReward(self):
        # only reward if the target career is reached
        # the smaller the cost, the higher the reward
        return (
            -np.inf
            if self.career != self.goal
            else -self.path.select(pl.col.cost).sum().item()
        )


# endregion
# example
# region: all careers
Lambda = careers.select(pl.col.career).unique().to_series()
k = np.random.choice(Lambda)
q = np.random.choice(Lambda)

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
searcher = mcts(timeLimit=1000)
action = searcher.search(initialState=pathfinder)


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

# dsds = pathfinder.getPossibleActions()

# _careers = pathfinder.careers.filter(pl.col.career == pathfinder.career).filter(
#     pl.col.careerTo == dsds["careerTo"]
# )

# _vertex = pathfinder.vertices.filter(pl.col.vertex == pathfinder.vertex)
# _vertexTo = pathfinder.vertices.filter(pl.col.vertex == dsds["vertexTo"])

# lalala = _cost(
#     ßkq=_careers["similarity"].item(),
#     xk=_vertex.select(pl.col.x).item(),
#     xq=_vertexTo.select(pl.col.x).item(),
#     tk=_vertex.select(pl.col.t).item(),
#     tq=_vertexTo.select(pl.col.t).item(),
# )

# lalala == pathfinder.cost(**dsds)

# pathfinder = pathfinder.takeAction(**dsds)

# pathfinder.career
# pathfinder.careers
# pathfinder.path
# pathfinder.vertex
# pathfinder.vertices

while not pathfinder.isTerminal():
    print(f"current vertex: {pathfinder.vertex}")
    dsds = pathfinder.getPossibleActions()
    pathfinder = pathfinder.takeAction(
        careerTo=dsds["careerTo"],
        vertexTo=dsds["vertexTo"],
    )
    # pathfinder = pathfinder.takeAction(**pathfinder.getPossibleActions())
    # print(f"current cost: {pathfinder.path}")

# endregion
