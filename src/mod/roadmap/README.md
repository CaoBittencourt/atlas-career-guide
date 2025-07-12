# `roadmap`: career roadmap utilities
## `goal`
The `goal` submodule helps users find out the optimal end-goal for their careers according to their own criteria.

## `path`
The `path` submodule optimizes career tracjectory to get the quickest and most optimal path towards an end-goal (often defined with the `goal` submodule).

## `plot`
The `path` submodule takes as input a career trajectory and outputs a plot to visualize one's professional journey.

## Example
One can combine these three modules to fully optimize their career as follows:
```r
# import roadmap module
box::use(roa = roadmap[...])

# read user's skills, preferences and strategies
read.csv('skill_set.csv') -> skill
read.csv('pref_set.csv') -> pref
read.csv('macro_strategy.csv') -> macro.str
read.csv('micro_strategy.csv') -> micro.str

# career roadmap
skill |> 
    roa$go$goal(
        prefs = pref,
        strategy = macro.str
    ) |> # user's optimal career (end-goal)
    roa$pa$which.vertex() |> 
    roa$pa$path() |> # user's optimal trajectory (path)
    roa$pl$plot.path() # plot user's career progression
```