# `path`: optimizing career progression
The `path` module allows users to find the quickest path from point A to point B in their career trajectory. This is meant to be coupled with the `goal` module, which helps them identify what this "point B" should be in order to aim towards it.

To acchieve this, the `path` submodule contains the following functions:
- `path`: optimizes career trajectory from a vertex to another, with optional utility for each vertex;
- `path.cost`: the cost (in years) for each step in the output of the `path` function;
- `path.util`: the (dimensionless) utility for each step in the output of the `path` function;
- `which.vertex`: all vertex ids in an occupation's career grid;

and, also, a `paths` object with the default vertex table and graph (as implemented in the `igraph` package).

## Example: starting from nothing
An example usage of this module is:
```r
# import path module
box::use(pa = roadmap / path)

# read occupations data
getOptions('atlas.ids') |> 
    readRDS() |> 
    # get a random occupation
    sample(1) |> 
    pa$which.vertex() |> 
    # find quickest path
    # from = null (default) means starting from nothing
    pa$path() -> 
prog

# calculate career progression cost in years
prog |> pa$path.cost() |> sum()

# calculate career progression utility
prog |> pa$path.util() |> sum()
```

## Example: starting from somewhere
If the user already has some experience, then this workflow is more appropriate:
```r
# import path module
box::use(pa = roadmap / path)

# user's data
list(
    occupation = 1 # user's occupation id
    , x = 5 # user's years of work experience in this career
    , t = 4 # user's years of education in this career
) -> user.data

# find closest vertex to user
user.data$occupation |> 
    pa$match.vertex(
        x = user.data$x,
        t = user.data$t
    ) ->
vertex.from

# read occupations data
getOptions('atlas.ids') |> 
    readRDS() |> 
    # get a random occupation
    sample(1) |> 
    pa$which.vertex() |> 
    # find quickest path
    # starting from "vertex.from"
    pa$path(vertex.from) -> 
prog

# calculate career progression cost in years
prog |> pa$path.cost() |> sum()

# calculate career progression utility
prog |> pa$path.util() |> sum()
```