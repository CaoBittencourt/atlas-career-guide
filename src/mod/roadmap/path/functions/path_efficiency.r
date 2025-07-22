# setup
# region: imports
box::use(
  gr = igraph,
  utils / data / last[...],
  roadmap / path / data[...],
  roadmap / path / functions / path_cost[...],
  roadmap / path / functions / base_cost[...],
)

# endregion
# dispatch
# region: get path efficiency (as a percentage of base cost)
path.efficiency <- function(
  epath,
  graph = paths$expected$graph,
  vertices = paths$expected$vertices
) {
  # assert args in main function
  # calculate path costs
  graph |>
    gr$get.edge.attribute(
      "vertex.to",
      epath
    ) |>
    last() |>
    vertex.cost(
      graph,
      vertices
    ) -> base.cost

  epath |>
    path.cost(graph) |>
    sum() -> epath.cost

  return(1 - (epath.cost / base.cost))
}

# endregion
# exports
# region: exports
box::export(path.efficiency)

# endregion
