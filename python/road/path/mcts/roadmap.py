# setup
# region: imports
from mcts import mcts
import polars as pl
import os
from pyreadr import read_r

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
# region: getPossibleActions()
# endregion
# region: takeAction(action)
# endregion
# region: isTerminal()
# endregion
# region: getReward()
# endregion
# region: state class

# endregion
# example
# region: 2 occupations

# endregion
# region: 20 occupations

# endregion
# region: all occupations

# endregion
