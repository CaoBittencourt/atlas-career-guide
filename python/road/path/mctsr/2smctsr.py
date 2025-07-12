# setup
# region: imports
from mcts import mcts
import polars as pl
import numpy as np
import os
from types import NoneType
from numbers import Number
from python.data import ist
from time import time
from copy import deepcopy

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

        # all careers progs
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

        self.years = 0

        # dead end flag
        self.deadEnd = False

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

        # from the documentation:
        # "In order to run MCTS, you must implement a State class which can fully describe the state of the world. It must also implement four methods:"

    # "getPossibleActions(): Returns an iterable of all actions which can be taken from this state"
    def getPossibleActions(self):
        # first stage:
        # feasible career progressions

        # expand sample with probabilities later
        # because there is no prob parameter
        # check out rollout policy parameter ins
        return (
            careers.filter(pl.col.career == self.career)
            .filter(
                ~pl.col.careerTo.is_in(
                    self.path.select(pl.col.career).to_series().to_list()
                )
            )
            .select(pl.col.careerTo)
            .to_series()
            .to_list()
        )

        # return (
        #     careers.filter(pl.col.career == self.career)
        #     .select(pl.col.careerTo)
        #     .to_series()
        #     .to_list()
        # )

    def cost(self, careerTo: int):
        # second stage:
        # feasible vertex progressions
        _vertexProgs = self.vertices.filter(pl.col.career == careerTo)

        # randomly select a vertex
        if _vertexProgs.is_empty():
            self.deadEnd = True
            return {
                "vertexTo": _vertexProgs,
                "cost": {
                    "work": 0,
                    "study": 0,
                    "total": 0 + 0,
                    "xReset": False,
                    "tReset": False,
                },
            }

        _vertexTo = self.vertices.filter(
            pl.col.vertex
            == np.random.choice(
                a=_vertexProgs.select(pl.col.vertex).to_series(),
                size=1,
                p=_vertexProgs.select(pl.col.prob).to_series()
                / _vertexProgs.select(pl.col.prob).sum().item(),
            ).item()
        )

        _vertex = self.vertices.filter(pl.col.vertex == self.vertex)

        return {
            "vertexTo": _vertexTo,
            "cost": _cost(
                ßkq=(
                    self.careers.filter(pl.col.career == self.career).filter(
                        pl.col.careerTo == careerTo
                    )
                )["similarity"].item(),
                xk=_vertex.select(pl.col.x).item(),
                xq=_vertexTo.select(pl.col.x).item(),
                tk=_vertex.select(pl.col.t).item(),
                tq=_vertexTo.select(pl.col.t).item(),
            ),
        }

    # "takeAction(action): Returns the state which results from taking action action"
    def takeAction(self, action: int):
        # movement cost
        _moveCost = self.cost(action)

        # new state
        newState = deepcopy(self)
        newState.path = pl.concat(
            [
                self.path,
                pl.DataFrame(
                    {
                        "career": action,
                        "x": _moveCost["vertexTo"].select(pl.col.x).item(),
                        "t": _moveCost["vertexTo"].select(pl.col.t).item(),
                        "xReset": _moveCost["cost"]["xReset"],
                        "tReset": _moveCost["cost"]["tReset"],
                        "cost": _moveCost["cost"]["total"],
                    }
                ),
            ]
        )

        # newState.careers = (
        #     self.careers.filter(pl.col.career != self.career)
        #     .filter(pl.col.careerTo != self.career)
        #     .filter(pl.col.careerTo != action)
        # )

        # newState.careers = (
        #     self.careers.filter(pl.col.career != self.career)
        #     .filter(pl.col.careerTo != self.career)
        #     .filter(pl.col.careerTo != action)
        # )

        newState.career = action
        newState.vertex = _moveCost["vertexTo"]["vertex"]
        newState.years = self.years + _moveCost["cost"]["total"]

        return newState

    # "isTerminal(): Returns whether this state is a terminal state"
    def isTerminal(self):
        # game ends when the target career is reached
        # or when maximum time is reached
        return any(
            [
                self.career == self.goal,
                self.years >= self.yearsMax,
                self.deadEnd,
            ]
        )

    # "getReward(): Returns the reward for this state. Only needed for terminal states."
    def getReward(self):
        # only reward if the target career is reached
        # the smaller the cost, the higher the reward
        return -self.years


# endregion
# example
# region: select careers
Lambda = careers.select(pl.col.career).unique().to_series()
k = np.random.choice(Lambda)
q = np.random.choice(Lambda)

optimizer = mcts(timeLimit=19000)
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

# endregion
# region: pathfinding
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
    pathfinder = pathfinder.takeAction(optimizer.search(pathfinder))

print(f"career path: {pathfinder.path}")
print(
    "could not reach goal career on time"
    if pathfinder.career != q
    else "reached goal career"
)

# endregion
