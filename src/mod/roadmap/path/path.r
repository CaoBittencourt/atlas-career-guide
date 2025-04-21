modular::project.options("atlas")
# setup
# region: imports
box::use(
  assert = mod / utils / assert,
  gr = igraph,
  mod / roadmap / path / data / graph[...],
  mod / roadmap / path / data / req[...],
  dplyr[...]
)

# endregion
# region: data
list(
  graph = paths.graph,
  table = paths
) -> paths

# endregion
# dispatch
# region: sketch
paths$
  table |>
  select(
    occupation
  ) |>
  unique() |>
  mutate(
    util = 0.19 # one non-negative number for each occupation
  ) |>
  inner_join(
    paths$table,
    by = c("occupation" = "occupation")
  ) |>
  mutate(
    cost_util = cost / util
  )

# endregion
# region: path cost
path.cost <- function(prog, paths.table) {

}

# endregion
# region: path util
path.util <- function(prog, util) {

}

# endregion
# region: path generic function
path <- function(from, to, util = rep(1, length(unique(paths$table$occupation))), graph = paths$graph) {
  # assert args
}

# endregion
# exports
# region: exports
box::export(
  path,
  path.cost,
  path.util,
  paths
)

# endregion
