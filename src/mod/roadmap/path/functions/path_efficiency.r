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
# region: path efficiency with expected graph
path.efficiency.expected <- function(epath) {
  # assert args in main function
  # dispatch path.efficiency
  return(
    epath |>
      path.efficiency(
        paths$expected$graph,
        paths$expected$vertices
      )
  )
}

# endregion
# region: path efficiency with detailed graph
path.efficiency.detailed <- function(epath) {
  # assert args in main function
  # dispatch path.efficiency
  return(
    epath |>
      path.efficiency(
        paths$detailed$graph,
        paths$detailed$vertices
      )
  )
}

# endregion
# exports
# region: exports
box::export(
  path.efficiency,
  path.efficiency.expected,
  path.efficiency.detailed
)

# endregion
